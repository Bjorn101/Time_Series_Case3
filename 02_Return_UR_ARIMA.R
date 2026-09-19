# --- Transform Price to Returns -------------

# Simple returns and log returns 
simple_ret_raw <- diff(bac$Adjusted) / bac$Adjusted[-length(bac$Adjusted)] # this means excluding the last observation
log_ret_raw <- diff(log(bac$Adjusted), differences = 1) 

simple_ret_raw[which(is.na(simple_ret_raw))] # first observation is missing
simple_ret <- na.omit(simple_ret_raw)
range(index(simple_ret)) # start on Tuesday

log_ret_raw[which(is.na(log_ret_raw))] # first observation is missing
log_ret <- na.omit(log_ret_raw)
range(index(log_ret)) # start on Tuesday

# summary statistics
summary(simple_ret) # max and min simple returns are -0.4, 0.26 
# so there is a significant difference between log and simple.
summary(log_ret)

plot(simple_ret, type = "l")
plot(log_ret, type = "l")

# --- Unit Root Testing --------
# We perform Unit Root testing to formally show that one time differencing of logged prices results in stationarity.

# We use the Pantula Principle (same as in 01)
log_ret_diff <- diff(log_ret) # actually twice differenced, since log_ret is one time differenced

# Pantula principle: start with d = 2
adf_diff2 <- adf(log_ret_diff, deterministics = "intercept")
adf_diff2   # reject h0

adf_diff1 <- adf(log_ret, deterministics = "intercept")
adf_diff1   # reject h0

adf_level <- adf(log(bac$Adjusted), deterministics = "intercept")
adf_level   # not reject h0 for d = 0 -> I(1) 

# This concludes that log_ret is actually stationary under the H1 with intercept

# --- Plotting correlograms for the full period ---------

par(mfrow = c(1,2))
acf(log_ret, main = "ACF of Logged Returns")
pacf(log_ret, main = "PACF of Logged Returns")
par(mfrow = c(1,1))

# Looking at the confidence bands:
# ACF: Ignoring lag 0, (zooming in) we do not see a clear cutoff in the autocorrelation. -> no MA model
# PACF: Although lag 1 is not partially correlated, we do observe partial correlation
# outside the 95% confidence bounds for lags 2, 3, 4, 5, 6, 9 etc.

# Plot lags up to 2 years
par(mfrow = c(1,2))
acf(log_ret, main = "ACF of Logged Returns", lag.max = 504)
pacf(log_ret, main = "PACF of Logged Returns", lag.max = 504)
par(mfrow = c(1,1))

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
ARMA00 <- arima(log_ret, order = c(0,0,0), method = "CSS", n.cond = 9, include.mean = TRUE) # this is simply the average logged return
summary(ARMA00)

AR6 <- arima(log_ret, order = c(6,0,0), method = "CSS", n.cond = 9, include.mean = TRUE) 
summary(AR6)

AR9 <- arima(log_ret, order = c(9,0,0), method = "CSS", n.cond = 9, include.mean = TRUE)
summary(AR9)

# ---- Automated ARIMA Estimation -----------

# Lastly, we can try the automated ARIMA to see if a better model is possible that results in stationary residuals.
library(forecast) # use auto.arima from the forecast package

# Can we find an ARIMA with a smaller lag than 9 but a higher AIC?
auto_arima_1 <- auto.arima(log_ret, max.p = 9, max.q = 9, ic = "aic") # use the AIC throughout this case to be consistent.
summary(auto_arima_1) # ARMA(5, 0, 6) with zero mean

ARMA75 <- arima(log_ret_clean, order = c(7,0,5), include.mean = FALSE, method = "CSS", n.cond = 9) # use the same method and n.cond for comparison
summary(ARMA75)

# ---- Model Performance Table ----------

models <- list(
  ARMA00 = ARMA00,
  AR6 = AR6,
  AR9 = AR9,
  ARMA75 = ARMA75
)

Model_Performance <- data.frame(
  AIC = sapply(models, compute_AIC),
  Sigma2 = sapply(models, function(x) x$sigma2)
)

Model_Performance

# ---- Forecast Performance Using ARMA56 ----------

split_train_test <- function(y, test_size) {
  T <- length(y)
  train <- window(y, start = time(y)[1], end = time(y)[T- (test_size)]) # window() preserves the time of ts.
  test <- window(y, start = time(y)[T- (test_size) + 1], end = time(y)[T]) # start and end are included.
  return(list(train = train, test = test))
}

compute_forecast_error <- function(train, test, order) {
  # estimate ARMA(1,2) model on training data
  model <- arima(train, order = order, method = "CSS")
  
  # ask Chatgpt: how to forecast an arima model in base R?
  pred <- predict(model, n.ahead = length(test))
  
  forecast <- pred$pred
  forecast_error <- forecast - test
  return(forecast_error)
}

compute_rolling_window_errors <- function(y, train_size, test_size, order) {
  latest_training_window_start <- length(y) - train_size - test_size + 1
  forecast_error_vector <- vector("list", latest_training_window_start)
  for (i in 1:latest_training_window_start) {
    data <- window(y, start = time(y)[i], end = time(y)[i + train_size + test_size - 1])
    split <- split_train_test(data, test_size)
    
    train <- split$train
    test <- split$test
    print(i)
    forecast_error_vector[[i]] <- compute_forecast_error(train, test, order)
  }
  return(forecast_error_vector)
}

train_size <- 1008 # 4 years (252 * 4)
test_size <- 1 # 1 day
order <- c(5,0,7) # ARMA57 had the best AIC
rolling_errors <- compute_rolling_window_errors(log_ret_clean, train_size, test_size, order)
errors_vector <- unlist(rolling_errors)
mean(errors_vector^2) 
var(log_ret_clean) # the variance is less than the mean-squared-error,
# this means that forecast a return of 0 would be better than forecasting with the ARMA57 model
# CONCLUSION: All our found ARMA models do not outperform a simple expection of 0 return.

