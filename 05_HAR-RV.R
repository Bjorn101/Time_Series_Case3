library(dplyr)
library(zoo)

## ---- 1. Import ----

# Use the bac data (xts) created in 00_Data_Extraction.R and convert it to a data frame,
# with the dates in the Index column
df <- data.frame(Index = index(bac), coredata(bac)) %>%
  arrange(Index)

head(df,10)

# Sanity check, all high's should be greater than the low's
stopifnot(all(df$High >= df$Low, na.rm = TRUE))
head(df)

## ---- 2. Daily log returns for realized range (log returns because of addition over time) ----
df <- df %>%
  mutate(log_ret = log(Adjusted / lag(Adjusted)))

## ---- 3. Realized range (Parkinson estimator) ----
# RR_t = (1/(4 log 2)) * (log(High_t) - log(Low_t))^2
# Range-based proxy for daily volatility, built directly from High/Low
df <- df %>%
  mutate(
    RR = (1 / (4 * log(2)) * (log(High) - log(Low))^2),
    RRVol = sqrt(RR)
  )

head(df, 10)

## ---- 4. HAR regressors: daily / weekly / monthly rolling averages (on volatility scale) ----
df <- df %>%
  mutate(
    RRVol_d = RRVol,
    RRVol_w = rollmean(RRVol, 5,  fill = NA, align = "right"),
    RRVol_m = rollmean(RRVol, 22, fill = NA, align = "right")
  )

head(df, 25)

## ---- 5. Build forecasting dataset and fit HAR-RR(Vol) via OLS ----
har_data <- df %>%
  mutate(RRVol_next = lead(RRVol)) %>%
  dplyr::select(Index, RRVol_next, RRVol_d, RRVol_w, RRVol_m) %>%
  na.omit()

har_model <- lm(RRVol_next ~ RRVol_d + RRVol_w + RRVol_m, data = har_data)
summary(har_model)

## ---- 6. Plot actual vs fitted ----
plot(har_data$Index, har_data$RRVol_next, type = "l",
     main = "HAR-RR: Actual vs Fitted (BAC Realized Volatility, Range-based)",
     xlab = "Date", ylab = "Realized Volatility (sqrt of Parkinson range)")
lines(har_data$Index, fitted(har_model), col = "red")
legend("topright", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = 1)