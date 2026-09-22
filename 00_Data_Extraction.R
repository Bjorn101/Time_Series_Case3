# install.packages("quantmod")

library(ggplot2)
library(quantmod)   # Tip from case description: to download data from Yahoo Finance
# quantmod also includes xts and zoo


## ---- Data collection -----------------------

# Ask Chatgpt: how to download the data of bank of america from yahoo finance using the quantmod package?
ticker     <- "BAC"
start_date <- "2000-01-03" # 1-1 is a saturday (no trading day), so we start at 3-1 (a monday).
end_date   <- "2026-09-11"

# Download daily OHLCV + Adjusted Close from Yahoo Finance
# getSymbols is from the quantmod package. it returns an xts object
getSymbols(ticker,
           src   = "yahoo",
           from  = start_date,
           to    = end_date,
           auto.assign = TRUE) # auto assign captures the data to the "BAC" variable

# getSymbols() creates an xts object called "BAC" in the environment
bac <- BAC
head(bac)
tail(bac)
str(bac)

# Original columns are: BAC.Open, BAC.High, BAC.Low, BAC.Close, BAC.Volume, BAC.Adjusted
colnames(bac) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

# Data quality checks
sum(is.na(bac))          # check for missing values
range(index(bac))        # check date range actually retrieved
nrow(bac)                # number of trading days obtained

# Conclusion: no missing data

# Ask Chatgpt: How to save xts data to a csv file?
write.zoo(bac, file = "BAC_daily.csv", sep = ",") 
# zoo (Z's Ordered Observations) package is loaded together with xts which is loaded with quantmod
# A zoo object is an ordered vector or matrix of data, each observation tagged with an index 
# (usually a date/time, but it can be any ordered type — even plain integers)

## ---- Plots & Basic Descriptive Statistics -------------------

plot(bac$Adjusted, type = "l")
summary(bac$Adjusted)
summary(bac$Volume)


# --- Event Plot --------------
# ask Chatgpt: Make a plot that visualizes the following events on the plot of the stock price: 
# Merrill Lynch Acquisition, Great Financial Crisis, Euro Debt Crisis, the Covid-19 Crash, US-IRAN war

# Define crisis windows as start/end dates
crisis_periods <- data.frame(
  name  = c("GFC", "Euro Debt Crisis", "COVID-19 Crash", "US-Iran War"),
  start = as.Date(c("2007-06-01", "2011-06-01", "2020-02-01", "2026-02-28")),
  end   = as.Date(c("2009-06-30", "2012-06-30", "2020-06-30", end_date)) # modified to allign with end_date of the data
)

merrill_date <- as.Date("2008-09-15")  # Merrill Lynch acquisition announcement

crisis_colors <- adjustcolor(c("red", "orange", "purple", "blue"), alpha.f = 0.2)

# Helper: shade the crisis windows on whichever panel is currently active
shade_crises <- function() {
  for (i in seq_len(nrow(crisis_periods))) {
    rect(xleft  = crisis_periods$start[i], xright = crisis_periods$end[i],
         ybottom = par("usr")[3], ytop = par("usr")[4],
         col = crisis_colors[i], border = NA)
  }
}

# --- Major Events Overlay --------------

plot(index(bac), as.numeric(bac$Adjusted), type = "n",
     xlab = "", ylab = "Adjusted Close Price",
     main = "BAC Adjusted Close")
shade_crises()
lines(index(bac), as.numeric(bac$Adjusted), col = "black", lwd = 1)
abline(v = merrill_date, col = "darkgreen", lty = 2, lwd = 2)
text(merrill_date, par("usr")[4], "Merrill Lynch\nacquisition",
     col = "darkgreen", cex = 0.7, pos = 1, offset = 0.3)
legend("topleft", legend = crisis_periods$name, fill = crisis_colors,
       border = NA, bty = "n", cex = 0.7)
