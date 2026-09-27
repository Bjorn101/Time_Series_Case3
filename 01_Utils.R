# --- Helper functions ---

# AIC computed manually for consistency across models fit with
# different n.cond (arima()'s built-in AIC is not available for the CSS method)
# across models fit via CSS with different conditioning samples)
compute_AIC <- function(model) {
  k <- length(model$coef) + 1
  loglik <- model$loglik
  return(2 * k - 2 * loglik)
}

# The functions below serve the out-of-sample forecasting test.

# Split a ts object into a training window and a subsequent test window
split_train_test <- function(y, test_size) {
  T <- length(y)
  train <- window(y, start = time(y)[1], end = time(y)[T - test_size])
  test <- window(y, start = time(y)[T - test_size + 1], end = time(y)[T])
  return(list(train = train, test = test))
}

# Fit an ARIMA(order) model on `train` and return the forecast error on `test`
# Non-zero mean is allowed.
compute_forecast_error <- function(train, test, order, mean) {
  model <- arima(train, order = order, method = "CSS", include.mean = mean)
  # ask Chatgpt: how to forecast an arima model in base R?
  pred <- predict(model, n.ahead = length(test))
  return(pred$pred - test)
}

# Rolling-window out-of-sample forecast errors for a given model order
compute_rolling_window_errors <- function(y, train_size, test_size, order, mean) {
  n_windows <- length(y) - train_size - test_size + 1
  errors <- vector("list", n_windows)
  for (i in seq_len(n_windows)) {
    window_data <- window(y, start = time(y)[i],
                          end = time(y)[i + train_size + test_size - 1])
    split <- split_train_test(window_data, test_size)
    errors[[i]] <- compute_forecast_error(split$train, split$test, order, mean)
  }
  return(errors)
}