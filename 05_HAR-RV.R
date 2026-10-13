library(dplyr)
library(zoo)

# --- 1. Data Preparation & Parkinson Realized Range ---

# Convert xts object to data frame preserving Date index
df <- data.frame(Index = index(bac), coredata(bac))

# Sanity check: High prices must strictly be greater than or equal to Low prices
stopifnot(all(df$High >= df$Low, na.rm = TRUE))

# Compute daily log returns
df <- df %>%
  mutate(log_ret = log(Adjusted / lag(Adjusted)))

# Parkinson (1980) High-Low Range Estimator:
# RR_t = (1 / (4 * log(2))) * (log(High_t) - log(Low_t))^2
df <- df %>%
  mutate(
    RR = (1 / (4 * log(2))) * (log(High) - log(Low))^2,
    RRVol = sqrt(RR)
  )

head(df, 10)


# --- 2. Part A: Daily HAR-RR Model Set-Up & Estimation ---

# Construct rolling average regressors: Daily (1d), Weekly (5d), Monthly (22d)
df <- df %>%
  mutate(
    RRVol_d = RRVol,
    RRVol_w = rollmean(RRVol, k = 5,  fill = NA, align = "right"),
    RRVol_m = rollmean(RRVol, k = 22, fill = NA, align = "right")
  )

head(df, 25)

# Build forecasting target dataset
har_data <- df %>%
  mutate(RRVol_next = lead(RRVol)) %>%
  dplyr::select(Index, RRVol_next, RRVol_d, RRVol_w, RRVol_m) %>%
  na.omit()

# Fit Daily HAR-RR via OLS
har_model <- lm(RRVol_next ~ RRVol_d + RRVol_w + RRVol_m, data = har_data)
summary(har_model)

# Plot actual vs fitted values
plot(
  har_data$Index, har_data$RRVol_next, type = "l", col = "black",
  main = "HAR-RR: Actual vs Fitted (BAC Realized Volatility, Range-based)",
  xlab = "Date", ylab = "Realized Volatility (sqrt of Parkinson range)"
)
lines(har_data$Index, fitted(har_model), col = "red")
legend("topright", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = 1)


# --- 3. Part B: Monthly Aggregated Realized Variance Set-Up ---

# Daily squared log returns act as the intramonth building block
df <- df %>%
  mutate(sq_ret = log_ret^2)

# Group by calendar month and sum daily squared returns
monthly_rv <- df %>%
  mutate(yearmon = format(Index, "%Y-%m")) %>%
  group_by(yearmon) %>%
  summarise(
    Index_month = max(Index),
    RV_monthly  = sum(sq_ret, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(Index_month)

head(monthly_rv, 10)

# Construct monthly HAR regressors: 1-Month (m1), 3-Month Quarter (q), 12-Month Year (y)
monthly_rv <- monthly_rv %>%
  mutate(
    RV_m1 = RV_monthly,
    RV_q  = rollmean(RV_monthly, k = 3,  fill = NA, align = "right"),
    RV_y  = rollmean(RV_monthly, k = 12, fill = NA, align = "right")
  )

head(monthly_rv, 15)

# Build forecasting dataset for next-month Realized Variance
har_monthly_data <- monthly_rv %>%
  mutate(RV_next = lead(RV_monthly)) %>%
  dplyr::select(Index_month, RV_next, RV_m1, RV_q, RV_y) %>%
  na.omit()


# --- 4. Monthly HAR Estimation & Model Comparison ---

# Model 1: Full Monthly HAR (1-month, 3-month quarter, 12-month year)
har_monthly_model1 <- lm(RV_next ~ RV_m1 + RV_q + RV_y, data = har_monthly_data)
summary(har_monthly_model1)

# Model 2: Reduced Monthly HAR (1-month, 3-month quarter)
har_monthly_model2 <- lm(RV_next ~ RV_m1 + RV_q, data = har_monthly_data)
summary(har_monthly_model2)

# Model 3: AR(1) Benchmark (1-month only)
har_monthly_model3 <- lm(RV_next ~ RV_m1, data = har_monthly_data)
summary(har_monthly_model3)
