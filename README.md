# Time Series Analysis – Case 3: Bank of America (BAC)

Case study for the course *Time Series Analysis*. We analyse the daily stock price of Bank of America (ticker `BAC`, 3 Jan 2000 – 11 Sep 2026, Yahoo Finance). We model the **level** of the price/returns with unit-root tests and ARIMA models, and the **volatility** of returns with GARCH-type models and a HAR model on realized range.

## How to run

Open `Project.Rproj` in RStudio and run the scripts **in numerical order, in the same R session**. The scripts don't `source()` each other. They rely on objects that earlier scripts leave in the global environment.

| Order | Script | Needs from earlier scripts | Creates / passes on |
|---|---|---|---|
| 1 | `00_Data_Extraction.R` | – | `bac` (xts with OHLCV + Adjusted) |
| 2 | `01_Utils.R` | – | helper functions `compute_AIC()`, `split_train_test()`, `compute_forecast_error()`, `compute_rolling_window_errors()` |
| 3 | `02_Price_UR_ARIMA.R` | `bac`, helper functions | ARIMA models for the price level |
| 4 | `03_Return_UR_ARIMA.R` | `bac`, helper functions | `log_ret`, ARMA models for log returns |
| 5 | `04_GARCH_Estimation.R` | `bac` | (G)ARCH / GJR / EGARCH fits |
| 6 | `05_HAR-RV.R` | `bac` | HAR model on realized range |

### Packages

`00_Data_Extraction.R` installs every package the project needs (`quantmod`, `ggplot2`, `bootUR`, `forecast`, `rugarch`, `dplyr`, `zoo`), but only the ones that aren't installed yet. The other scripts only load them with `library()`.

## What each script does

### `00_Data_Extraction.R`: packages and data
Installs any missing packages for the whole project, then downloads daily BAC data from Yahoo Finance with `quantmod::getSymbols()`, renames the columns, checks for missing values (there are none) and stores the data as `bac`, which all later scripts use. A copy is also saved to `BAC_daily.csv`. It also plots the adjusted close with the major events shaded: the Merrill Lynch acquisition, the GFC, the Euro debt crisis, the COVID-19 crash and the US–Iran war.

### `01_Utils.R`: helper functions
Reusable functions for the ARIMA scripts:
- `compute_AIC()`: AIC computed by hand, because `arima(..., method = "CSS")` doesn't report one.
- `split_train_test()`, `compute_forecast_error()`, `compute_rolling_window_errors()`: rolling-window, out-of-sample 1-step-ahead forecast errors for a given ARIMA order.

### `02_Price_UR_ARIMA.R`: price level
- ADF and bootstrap ADF tests (`bootUR`) following the Pantula principle (d = 2 → 1 → 0). The adjusted price is **I(1)**.
- ACF/PACF of the first differences, then ARIMA(0,1,0), (1,1,0) and (0,1,1) are compared by AIC. The result is checked with `auto.arima`, which also picks ARIMA(0,1,0), i.e. a random walk.
- Rolling-window out-of-sample MSE comparison.

### `03_Return_UR_ARIMA.R`: returns
- Builds simple and log returns. All further modelling uses **log returns** (`log_ret`).
- Pantula-principle ADF and bootstrap ADF tests: the log price is I(1) and the log returns are stationary.
- ACF/PACF give no clean cut-off, so we compare ARMA(0,0), AR(6), AR(9) and the `auto.arima` choice ARMA(7,5) by AIC and by rolling-window out-of-sample MSE against a no-model benchmark.
- ARMA(0,0) (mean only) forecasts best. Its residuals still show **volatility clustering**, which motivates the volatility models in scripts 04 and 05.

### `04_GARCH_Estimation.R`: conditional volatility
- Uses log returns in percent. The ACF of absolute returns shows why an ARMA model alone isn't enough.
- Estimates GARCH(1,1) with normal and Student-t errors. The t shape parameter is ≈ 5.3, which means fat tails, so Student-t is used from here on.
- Estimates ARCH(1), ARCH(2), GARCH(1,2) and the asymmetric GJR-GARCH(1,1) and EGARCH(1,1) (Engle & Ng, 1993).
- Compares the models by AIC/BIC. **EGARCH(1,1)** is selected: it has the lowest AIC/BIC and captures the asymmetric response to good and bad news.

### `05_HAR-RV.R`: realized volatility
- Converts `bac` (from script 00) to a data frame.
- Builds a range-based realized volatility with the Parkinson estimator, RR = (log High − log Low)² / (4 log 2).
- Fits a HAR model (daily, weekly = 5-day and monthly = 22-day averages) with OLS for next-day volatility, and plots actual vs fitted.

## How the scripts connect

```
00_Data_Extraction ──► bac ──┬──► 02_Price_UR_ARIMA   (price is I(1) → random walk)
                             ├──► 03_Return_UR_ARIMA  (log returns stationary, mean-only model;
                             │                         residuals show volatility clustering)
                             ├──► 04_GARCH_Estimation (model that clustering: EGARCH)
                             └──► 05_HAR-RV           (alternative volatility model on realized range)
01_Utils ──► helper functions used by 02 and 03
```

The story: prices are non-stationary (02), so we model returns (03). Returns are hard to predict in the mean, but their variance is predictable. That leads to GARCH-type models (04) and a HAR model for realized volatility (05).
