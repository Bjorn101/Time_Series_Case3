setwd("C:/Users/inesr/Desktop/Time Series Analytics")
bac <- read.csv("BAC_daily.csv")

# Look at the data
head(bac)
str(bac)
summary(bac)

bac$Index <- as.Date(bac$Index) # Convert Index to date format
str(bac) # Check the structure

# Plot adjusted stock price
plot(bac$Index, bac$Adjusted,
     type = "l",
     main = "Bank of America Adjusted Stock Price",
     xlab = "Date",
     ylab = "Adjusted Price (USD)")
# Descriptive statistics of adjusted price serie
mean(bac$Adjusted)
sd(bac$Adjusted)
min(bac$Adjusted)
max(bac$Adjusted)

acf(bac$Adjusted, main = "ACF of Bank of America Adjusted Stock Price") #autocorrelation

##(2)  Unit Root test
#install.packages("bootUR")
library(bootUR)

# Create first and second differences
bac_diff1 <- diff(bac$Adjusted)
bac_diff2 <- diff(bac_diff1)

# Pantula principle: start with d = 2
adf_diff2 <- adf(bac_diff2,deterministics = "intercept")
adf_diff2   #reject h0

adf_diff1 <- adf(bac_diff1, deterministics = "intercept")
adf_diff1   #reject h0

adf_level <- adf(bac$Adjusted, deterministics = "intercept")
adf_level   #not reject h0-> I(1) 

## Bootstrap ADF Unit Root Test
boot_adf_diff2 <- boot_adf(bac_diff2, deterministics = "intercept")
boot_adf_diff2 #reject h0

boot_adf_diff1 <- boot_adf(bac_diff1, deterministics = "intercept")
boot_adf_diff1  #reject h0

boot_adf_level_int <- boot_adf(bac$Adjusted, deterministics = "intercept")
boot_adf_level_int

# Bootstrap ADF with trend
boot_adf_level_trend <- boot_adf(bac$Adjusted, deterministics = "trend")
boot_adf_level_trend


#### (3)  ARIMA for price series

acf(bac_diff1, main = "ACF of First-Differenced Adjusted Price")
pacf(bac_diff1, main = "PACF of First-Differenced Adjusted Price")

mean(bac_diff1)

## Manual ARIMA Models
#install.packages("forecast")
library(forecast)

arima_010 <- Arima(bac$Adjusted, order = c(0, 1, 0), include.drift = F)
arima_110 <- Arima(bac$Adjusted, order = c(1, 1, 0), include.drift = F)
arima_011 <- Arima(bac$Adjusted, order = c(0, 1, 1), include.drift = F)

arima_010 #selected
arima_110
arima_011
arimad_010 <- Arima(bac$Adjusted, order = c(0, 1, 0), include.drift = T)
arimad_010
checkresiduals(arima_010) #detects no autocorrelation

## Automatic ARIMA Model Selection
auto_arima <- auto.arima(bac$Adjusted)
auto_arima  #selects same model (0,1,0)
checkresiduals(auto_arima)
