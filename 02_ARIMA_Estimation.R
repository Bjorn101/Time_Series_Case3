# --- Plotting correlograms for the full period ---------

# plotting ACF and PACF in a single figure
plot_acf_pacf <- function(series){
  par(mfrow = c(1,2)) # to plot both in a single figure
  
  # Get the name of the series (after debugging with Chatgpt)
  series_name <- deparse(substitute(series))
  
  # Correlogram (ACF)
  acf(series, main = paste("ACF of", series_name))
  
  # Partial correlogram (PACF)
  pacf(series, main = paste("PACF of", series_name))
  
  # reset par
  par(mfrow = c(1,1))
}

plot(log_ret_clean, type = "l") # The variance changes over time.
plot_acf_pacf(log_ret_clean) 

# Looking at the confidence bands:
# ACF: Ignoring lag 0, (zooming in) we do not see a clear cutoff in the autocorrelation. -> no MA model
# PACF: Although lag 1 is not partially correlated, we do observe partial correlation
# outside the 95% confidence bounds for lags 2, 3, 4, 5, 6, 9 etc.

# --- AIC Function -----------

compute_AIC <- function(model) {
  k <- length(model$coef) + 1
  loglik <- model$loglik
  AIC <- 2*k-2*loglik
  return(AIC)
}

# --- Manual ARIMA Estimation --------

# Although there is no clear cutoff in the PACF or the ACF. There is autocorrelation in several lags.
# We try 3 models: ARMA(0,0) (so only the constant), AR(6) and AR(9)
# We define n.cond to the maximum lag (9) to ensure a comparison of the AIC values (all models are tested on the same sample size)
# We use CSS (conditional sum of squares) as defined in class.
# We allow for a constant because by efficient market the return is expected to be equal to the risk free rate.
ARMA00 <- arima(log_ret_clean, order = c(0,0,0), method = "CSS", n.cond = 9, include.mean = FALSE) # this is simply the average logged return
summary(ARMA00)

AR6 <- arima(log_ret_clean, order = c(6,0,0), method = "CSS", n.cond = 9) 
summary(AR6)

AR9 <- arima(log_ret_clean, order = c(9,0,0), method = "CSS", n.cond = 9)
summary(AR9)

# ---- Automated ARIMA Estimation -----------

# Lastly, we can try the automated ARIMA to see if a better model is possible that results in stationary residuals.
library(forecast) # use auto.arima from the forecast package

# Can we find an ARIMA with a smaller lag than 9 but a higher AIC?
auto_arima_1 <- auto.arima(log_ret_clean, max.p = 9, max.q = 9, ic = "aic") # use the AIC throughout this case to be consistent.
summary(auto_arima_1) # ARMA(5, 0, 6) with zero mean

# Can we find an ARIMA with a smaller lag than 6 but a higher AIC?
auto_arima_2 <- auto.arima(log_ret_clean, max.p = 6, max.q = 6, ic = "aic") # use the AIC throughout this case to be consistent.
summary(auto_arima_2) # ARMA(5, 0, 6) with zero mean

ARMA56 <- arima(log_ret_clean, order = c(5,0,6), include.mean = FALSE, method = "CSS", n.cond = 9) # use the same method and n.cond for comparison
summary(ARMA56)

# ---- Model Performance Table ----------

models <- list(
  ARMA00 = ARMA00,
  AR6 = AR6,
  AR9 = AR9,
  ARMA56 = ARMA56
)

Model_Performance <- data.frame(
  model_name = names(models),
  AIC = sapply(models, compute_AIC),
  Sigma2 = sapply(models, function(x) x$sigma2)
)

Model_Performance