library(bootUR)
library(forecast)

# --- Plotting price and correlograms for the full period ---
# Plot adjusted stock price

par(mfrow = c(1,2))
acf(bac$Adjusted, main = "ACF of Bank of America Adjusted Stock Price") # autocorrelation
pacf(bac$Adjusted, main = "ACF of Bank of America Adjusted Stock Price") # partial autocorrelation
par(mfrow = c(1,1))
  
# --- Unit Root Testing ---

# Create first and second differences
bac_diff1 <- diff(bac$Adjusted)
bac_diff2 <- diff(bac_diff1)

which(is.na(bac_diff1))
which(is.na(bac_diff2))

# remove the first observation for which there is nothing to difference against.
bac_diff1 <- na.omit(bac_diff1)
bac_diff2 <- na.omit(bac_diff2)

# Pantula principle: start with d = 2
adf_diff2 <- adf(bac_diff2, deterministics = "none")
adf_diff2   # reject h0

adf_diff1 <- adf(bac_diff1, deterministics = "intercept")
adf_diff1   # reject h0

adf_level <- adf(bac$Adjusted, deterministics = "trend")
adf_level   # not reject h0 for d = 0 -> I(1) 

# --- Bootstrap ADF Unit Root Test ---
boot_adf_diff2 <- boot_adf(bac_diff2, deterministics = "none")
boot_adf_diff2 # reject h0

boot_adf_diff1 <- boot_adf(bac_diff1, deterministics = "intercept")
boot_adf_diff1  # reject h0

boot_adf_level_int <- boot_adf(bac$Adjusted, deterministics = "trend")
boot_adf_level_int # not reject h0


# --- ARIMA for price series ---


par(mfrow = c(1,2))
acf(bac_diff1, main = "ACF of First-Differenced Adjusted Price")
pacf(bac_diff1, main = "PACF of First-Differenced Adjusted Price")
par(mfrow = c(1,1))

mean(bac_diff1)

# --- Manual ARIMA Models ---
# n.cond is set to 1 to ensure that all models use the same sample data (for AIC comparison)
arima_010 <- arima(bac$Adjusted, order = c(0, 1, 0), n.cond = 1, method = "CSS", include.mean = TRUE) # arima from stats package
arima_110 <- arima(bac$Adjusted, order = c(1, 1, 0), n.cond = 1, method = "CSS", include.mean = TRUE)
arima_011 <- arima(bac$Adjusted, order = c(0, 1, 1), n.cond = 1, method = "CSS", include.mean = TRUE)

summary(arima_010) # selected
summary(arima_110)
summary(arima_011)

# --- Model Performance Table ---

models <- list(
  arima_010 = arima_010,
  arima_110 = arima_110,
  arima_011 = arima_011
)

Model_Performance <- data.frame(
  AIC = sapply(models, compute_AIC),
  Sigma2 = sapply(models, function(x) x$sigma2)
)

Model_Performance

# --- Automated Arima ---

auto_arima <- auto.arima(bac$Adjusted, ic = "aic", method = "CSS") # from forecast package
summary(auto_arima)  # selects same model (0,1,0)
checkresiduals(auto_arima)

# --- Out-of-sample forecast evaluation ---

train_size <- round(0.7*length(bac$Adjusted))  # 70% of the data
test_size <- 1 # 1-day-ahead forecast

# ask ChatGPT: How to efficiently apply the compute_rolling_window_errors function on a list of arima models 
# with varying orders and zero/non-zero means?
orders <- list(
  "ARIMA(0,1,0)" = list(order = c(0, 1, 0), mean = TRUE),
  "ARIMA(1,1,0)" = list(order = c(1, 1, 0), mean = TRUE),
  "ARIMA(0,1,1)" = list(order = c(0, 1, 1), mean = TRUE)
)

rolling_errors_prices <- lapply(orders, function(order) {
  compute_rolling_window_errors(bac$Adjusted, train_size, test_size, order = order$order, mean = order$mean)
})

mse_results_prices <- data.frame(
  model = names(rolling_errors_prices),
  MSE = sapply(rolling_errors_prices, function(e) mean(unlist(e)^2))
)
mse_results_prices



# all of the models' MSE's are smaller than the MSE of no model. 
# The lowest model MSE belongs to ARMA(0,0) (only the intercept)