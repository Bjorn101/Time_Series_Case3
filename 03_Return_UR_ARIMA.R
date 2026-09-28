library(forecast) # for auto.arima function
library(bootUR) # for adf function



# --- Data preparation: price to returns ---

# Simple and log returns
simple_ret_raw <- diff(bac$Adjusted) / bac$Adjusted[-length(bac$Adjusted)] # in the denominator: the last element is dropped
log_ret_raw <- diff(log(bac$Adjusted), differences = 1)

# Both series have a missing first observation (nothing to difference against for the first observation) -> we drop it
simple_ret <- na.omit(simple_ret_raw)
log_ret <- na.omit(log_ret_raw)

range(index(simple_ret))  # sample starts on a Tuesday
range(index(log_ret)) # sample starts on a Tuesday

# --- Exploratory analysis ---

summary(simple_ret)   # max/min simple returns (~-0.40 / 0.26) diverge
summary(log_ret)      # from log returns (simple vs. log returns differ for large moves)

plot(simple_ret, type = "l", main = "Simple Returns")
plot(log_ret,    type = "l", main = "Log Returns")

# --- Unit root testing (Pantula Principle) ---

# We perform a UR test to formally confirm that log price is I(1), i.e. one round of
# differencing (= log returns) is stationary. 
# We use the Pantula Principle

log_ret_diff <- diff(log_ret) # second difference of log price

adf_log_ret_diff <- adf(log_ret_diff, deterministics = "none") # adf from bootUR package
adf_log_ret_diff  # d = 2: reject H0 (no unit root left at this order)

adf_log_ret <- adf(log_ret, deterministics = "intercept")
adf_log_ret  # d = 1: reject H0 -> log returns are stationary

adf_log_price <- adf(log(bac$Adjusted), deterministics = "trend")
adf_log_price  # d = 0: fail to reject H0 -> log price itself is nonstationary -> log_price is I(1)

# Bootstrap ADF Unit Root Test (more robust against heteroskedasticity)
boot_adf_log_ret_diff <- boot_adf(log_ret_diff, deterministics = "none")
boot_adf_log_ret_diff # reject h0

boot_log_ret <- boot_adf(log_ret, deterministics = "intercept")
boot_log_ret  # reject h0

boot_adf_log_price <- boot_adf(log(bac$Adjusted), deterministics = "trend")
boot_adf_log_price # not reject h0 -> log_price is I(1)

# log price is I(1); log returns (log_ret) are stationary.
# All subsequent modeling is done on log_ret.

# --- Correlogram analysis: candidate model orders ---

par(mfrow = c(1, 2))
acf(log_ret,  main = "ACF of Log Returns")
pacf(log_ret, main = "PACF of Log Returns")
par(mfrow = c(1, 1))

# No clear ACF cutoff -> no clean MA order.
# PACF shows lag 1 insignificant but partial correlation outside the
# 95% bands at lags 2, 3, 4, 5, 6, 9, etc. -> no clean AR order either.

# --- Model estimation ---

# Since neither correlogram gives a clean cutoff, compare a small set
# of candidate models. 
# n.cond is fixed at 9 (the largest lag order considered) across all models 
# -> their likelihoods/AICs are computed on the same effective sample and are directly comparable.
# Non-zero mean is allowed: under the efficient market, expected returns are at least the risk-free rate.

ARMA00 <- arima(log_ret, order = c(0, 0, 0), method = "CSS", n.cond = 9, include.mean = TRUE) # arima from stats package
AR6 <- arima(log_ret, order = c(6, 0, 0), method = "CSS", n.cond = 9, include.mean = TRUE)
AR9 <- arima(log_ret, order = c(9, 0, 0), method = "CSS", n.cond = 9, include.mean = TRUE)

summary(ARMA00) 

summary(AR6)

summary(AR9)

# Let auto.arima search for a better model (set max lag to one larger than our manual models)
auto_arima_log_ret <- auto.arima(log_ret, max.p = 10, max.q = 10, ic = "aic", allowmean = TRUE, method = "CSS")
summary(auto_arima_log_ret)  # selects ARMA(7,0,5) with non-zero mean

best_order <- arimaorder(auto_arima_log_ret)[c("p", "d", "q")]  

# Re-estimate with the same method/n.cond as the other candidates for a fair AIC comparison
ARMA75 <- arima(log_ret, order = best_order, include.mean = FALSE, method = "CSS", n.cond = 9)
summary(ARMA75)

# --- Model comparison table ---

models <- list(ARMA00 = ARMA00, AR6 = AR6, AR9 = AR9, ARMA75 = ARMA75)

# ask Chatgpt: How to summarize the AIC and the sigma2 of a list of models in a data frame?
model_performance <- data.frame(
  AIC = sapply(models, compute_AIC),
  Sigma2 = sapply(models, function(m) m$sigma2) # todo: remove
)
model_performance

# --- Out-of-sample forecast evaluation ---

train_size <- round(0.7*length(log_ret))  # 70% of the data 
test_size <- 1 # 1-day-ahead forecast

# ask ChatGPT: How to efficiently apply the compute_rolling_window_errors function on a list of arima models 
# with varying orders and zero/non-zero means?
orders <- list(
  "ARMA(0,0)" = list(order = c(0, 0, 0),        mean = TRUE),
  "AR(6)"     = list(order = c(6, 0, 0),        mean = TRUE),
  "AR(9)"     = list(order = c(9, 0, 0),        mean = TRUE),
  "ARMA(7,5)" = list(order = as.numeric(best_order), mean = FALSE)  # zero mean
)

rolling_errors <- lapply(orders, function(order) {
  compute_rolling_window_errors(log_ret, train_size, test_size, order = order$order, mean = order$mean)
})

mse_results <- data.frame(
  model = names(rolling_errors),
  MSE = sapply(rolling_errors, function(e) mean(unlist(e)^2))
)
mse_results

# No model benchmark
no_model_mse <- var(log_ret)
no_model_mse


# all of the models' MSE's are smaller than the MSE of no model. 
# The lowest model MSE belongs to ARMA(0,0) (only the intercept)

checkresiduals(ARMA00) # volatility clustering remains -> continue with GARCH and HAR models to forecast volatility
