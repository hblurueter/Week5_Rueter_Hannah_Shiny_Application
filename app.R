library(fpp3)
library(urca)
library(shiny)

wine_sales <- readr::read_csv(
  "AustralianWines.csv",
  trim_ws = TRUE,
  show_col_types = FALSE
) |>
  rename_with(trimws) |>
  rename(Sparkling = sparkling) |>
  mutate(
    Month = yearmonth(
      as.Date(paste0("1-", sub("-", "-19", Month)), format = "%d-%b-%Y")
    ),
    across(
      -Month,
      ~ readr::parse_double(as.character(.x), na = c("", "NA", "*"))
    )
  ) |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  as_tsibble(index = Month, key = Varietal)


sales_dates <- as.Date(wine_sales$Month)

ui <- fluidPage(
  titlePanel("Australian Wine Sales"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "varietals", "Wine varietals",
        choices = sort(unique(wine_sales$Varietal)),
        selected = c("Sparkling", "Fortified"),
        multiple = TRUE
      ),
      dateRangeInput(
        "dates", "Sales date range",
        start = min(sales_dates),
        end = max(sales_dates),
        min(sales_dates),
        max(sales_dates)
      ),
      sliderInput(
        "validation_months", "Months held out for validation",
        min = 1, max = 24, value = 12, step = 1
      ),
      sliderInput(
        "forecast_months", "Months to forecast ahead",
        min = 1, max = 24, value = 12, step = 1
      ),
      checkboxGroupInput(
        "display_models", "Models to show on forecast plots",
        choices = c("TSLM", "ETS", "ARIMA"),
        selected = c("TSLM", "ETS", "ARIMA")
      )
    ),
    
    mainPanel(
      textOutput("split_summary"),
      plotOutput("sales_plot", height = "450px"),
      h4("Models fitted on the training data"),
      verbatimTextOutput("model_fits"), 
      h4("Forecast accuracy"),
      tableOutput("accuracy_table"),
      h4("Forecasts vs. actual validation sales"),
      plotOutput("validation_plot", height = "450px"),
      h4("Future forecasts"),
      plotOutput("future_plot", height = "450px")
    )
  )
)

server <- function(input, output, session) {
  selected_sales <- reactive({
    validate(need(length(input$varietals) > 0, "Select at least one varietal."))
    req(input$dates[1], input$dates[2])
    
    sales <- wine_sales |>
      filter(
        Varietal %in% input$varietals,
        Month >= yearmonth(input$dates[1]),
        Month <= yearmonth(input$dates[2])
      )
    validate(need(nrow(sales) > 0, "Choose a date range with sales data."))
    sales
  })
  
  sales_split <- reactive({
    sales <- selected_sales()
    months <- sort(unique(sales$Month))
    holdout <- input$validation_months
    
    validate(need(
      length(months) > holdout,
      "Choose a longer date range or hold out fewer months."
    ))
    
    cutoff <- months[length(months) - holdout]
    
    list(
      train = filter(sales, Month <= cutoff),
      validation = filter(sales, Month > cutoff)
    )
  })
  
  training_fits <- reactive({
    train <- sales_split()$train
    
    validate(need(
      length(unique(train$Month)) >= 24,
      "Choose a date range that leaves at least 24 training months."
    ))
    
    train <- train |>
      group_by(Varietal) |>
      mutate(
        Sales = coalesce(Sales, dplyr::lag(Sales, 12L, order_by = Month))
      ) |>
      ungroup()
    
    validate(need(
      !anyNA(train$Sales),
      "The training window contains missing sales. Adjust the dates or varietals."
    ))
    
    train |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales),
        .safely = FALSE
      )
  })
  
  output$model_fits <- renderPrint({
    training_fits()
  })
  
  validation_forecasts <- reactive({
    validation_dates <- sales_split()$validation |>
      select(-Sales)
    
    training_fits() |>
      forecast(new_data = validation_dates)
  })
  
  output$accuracy_table <- renderTable({
    training_actuals <- sales_split()$train |>
      as_tibble() |>
      select(Varietal, Month, Actual = Sales)
    
    training_scores <- training_fits() |>
      fabletools::augment() |>
      as_tibble() |>
      inner_join(training_actuals, by = c("Varietal", "Month")) |>
      filter(!is.na(Actual), is.finite(.fitted)) |>
      mutate(Error = Actual - .fitted) |>
      group_by(Varietal, .model) |>
      summarise(
        RMSE = sqrt(mean(Error^2)),
        MAE = mean(abs(Error)),
        MAPE = 100 * mean(abs(Error[Actual != 0] / Actual[Actual != 0])),
        .groups = "drop"
      ) |>
      mutate(Window = "Training")
    
    validation_scores <- fabletools::accuracy(
      validation_forecasts(),
      data = selected_sales()
    ) |>
      mutate(Window = "Validation")
    
    bind_rows(training_scores, validation_scores) |>
      select(Varietal, Model = .model, Window, RMSE, MAE, MAPE) |>
      group_by(Varietal, Window) |>
      mutate(
        Result = if_else(
          Window == "Validation" & dense_rank(RMSE) == 1L,
          "Lowest validation RMSE",
          "",
          missing = ""
        )
      ) |>
      ungroup() |>
      arrange(Varietal, Window, RMSE)
  }, digits = 2)
  
  output$validation_plot <- renderPlot({
    validate(need(
      length(input$display_models) > 0,
      "Select at least one model to display."
    ))
    actuals <- sales_split()$validation
    
    validation_forecasts() |>
      filter(.model %in% input$display_models) |>
      autoplot(level = 95) +
      geom_line(
        data = actuals,
        aes(x = Month, y = Sales),
        inherit.aes = FALSE,
        colour = "black",
        na.rm = TRUE
      ) +
      geom_point(
        data = actuals,
        aes(x = Month, y = Sales),
        inherit.aes = FALSE,
        colour = "black",
        size = 1,
        na.rm = TRUE
      ) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      labs(
        x = "Month",
        y = "Sales",
        subtitle = "Black points show actual sales. Shading shows 95% prediction intervals."
      )
  })
  
  output$split_summary <- renderText({
    split <- sales_split()
    
    paste0(
      "Training: ", min(split$train$Month), " to ", max(split$train$Month),
      " | Validation: ", min(split$validation$Month),
      " to ", max(split$validation$Month)
    )
  })
  
  output$sales_plot <- renderPlot({
    selected_sales() |>
      autoplot(Sales) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      labs(x = "Month", y = "Sales") +
      guides(colour = "none")
  })
  
  future_fits <- reactive({
    sales <- selected_sales()
    
    validate(need(
      length(unique(sales$Month)) >= 24,
      "Choose at least 24 months of sales to fit the models."
    ))
    sales <- sales |>
      group_by(Varietal) |>
      mutate(
        Sales = coalesce(Sales, dplyr::lag(Sales, 12L, order_by = Month))
      ) |>
      ungroup()
    
    validate(need(
      !anyNA(sales$Sales),
      "The selected data contain missing sales. Adjust the dates or varietals."
    ))
    
    sales |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })
  
  output$future_plot <- renderPlot({
    validate(need(
      length(input$display_models) > 0,
      "Select at least one model to display."
    ))
    sales <- selected_sales()
    last_month <- max(sales$Month)
    recent_sales <- filter(sales, Month >= last_month - 35)
    
    future_fits() |>
      forecast(h = input$forecast_months) |>
      filter(.model %in% input$display_models) |>
      autoplot(recent_sales, level = 95) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      labs(
        x = "Month",
        y = "Sales",
        colour = "Model",
        fill = "Model",
        subtitle = "Shading shows 95% prediction intervals."
      )
  })
}

shinyApp(ui = ui, server = server)
