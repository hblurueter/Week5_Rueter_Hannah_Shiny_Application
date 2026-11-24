# Week5_Rueter_Hannah_Shiny_Application
Shiny application that uses Australian Wne sales and comparing TSLM, ETS and ARIMA forecasts by varietal.

This Shiny app was built for ADS 506, Applied Time Series Analysis.
[Launch the live Shiny app](https://67sk8r-hannah-rueter.shinyapps.io/Week5_Rueter_Hannah/)

## App features

- Compare multiple wine varietals and adjust the historical date range.
- - Set the validation window and future forecast horizon independently.
- Review model specifications and compare training and validation RMSE, MAE, and MAPE.
- See which model has the lowest validation RMSE for each varietal.
- Toggle models on the validation and future forecast plots, which show 95% prediction intervals.

## Run locally

1. Download `app.R`.
2. Place `AustralianWines.csv` in the same folder as `app.R`. The dataset is available in the ADS 506 Week 5 assignment materials.
3. In RStudio, install the packages below if needed. Open `app.R` and click **Run App**.

```r
install.packages(c("shiny", "fpp3"))
```

## References and acknowledgments

- Hyndman, R. J., & Athanasopoulos, G. (2021). *Forecasting: Principles and Practice* (3rd ed.). The app follows the textbook examples for [creating a monthly tsibble](https://otexts.com/fpp3/graphics.html) and [evaluating forecast accuracy](https://otexts.com/fpp3/accuracy.html).
- Posit’s [Shiny tutorial on reactive expressions](https://shiny.posit.co/r/getstarted/shiny-basics/lesson6/) provides a reference for connecting user inputs to plots and tables.
- The `fabletools` documentation provides references for [generating forecasts](https://fabletools.tidyverts.org/reference/forecast.html) and [plotting prediction intervals](https://fabletools.tidyverts.org/reference/autoplot.fbl_ts.html).

I used ChatGPT to help draft and debug portions of the app. I tested the app in RStudio and verified that the deployed app worked on shinyapps.io.
