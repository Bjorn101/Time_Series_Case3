library(vars)                   
library(frequencyConnectedness) # this includes the functions for the spillover analysis
library(quantmod)               # get data with getSymbols() for the other banks


# --- Settings ---
H <- 10 # forecast horizon as in DY (2012)
window_size <- 200# rolling window length as in DY (2012)

# askChatgpt: retrieve the daily logged range of the six largest US banks by assets-under-management. (We already have bac)
# --- Data: log range-based volatility of six large US banks (Bank of America, JP Morgan, Citi, Wells Fargo, Goldman Sachs, Morgan Stanley) ---

tickers <- c("BAC", "JPM", "C", "WFC", "GS", "MS")

banks <- list(BAC = bac) # BAC from script 00
for (tk in setdiff(tickers, "BAC")) {
  x <- getSymbols(tk, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  banks[[tk]] <- x
}

# Range-based volatility, in logs: log( sqrt( (log H - log L)^2 / (4 log 2) ) )
log_range_vol <- function(x) {
  hl <- as.numeric(log(x$High) - log(x$Low))
  hl[hl <= 0] <- NA # High == Low -> log(0) -> treat as missing
  xts(log(sqrt(hl^2 / (4 * log(2)))), order.by = index(x))
}

# Put the log volatility of all banks next to each other in one xts object.
# Start with BAC and add the other banks one by one; merge() matches the rows on date.
vol <- log_range_vol(banks[["BAC"]])
for (tk in setdiff(names(banks), "BAC")) {
  vol <- merge(vol, log_range_vol(banks[[tk]]))
}
colnames(vol) <- names(banks)
vol <- na.omit(vol) # common trading days only
range(index(vol))
nrow(vol)


# --- Full-sample VAR ---
lag_sel <- VARselect(vol, lag.max = 22, type = "const") # using vars package
lag_sel$selection
p_var <- as.numeric(lag_sel$selection["SC(n)"]) # BIC: not AIC, because we care more about efficiency. AIC would make the VAR too complex.

var_full <- VAR(vol, p = p_var, type = "const")

# --- Spillover table with the package ---
# askChatgpt: perform a spillover analysis according to the paper (DY (2012)).

sp_full <- spilloverDY12(var_full, n.ahead = H - 1, no.corr = FALSE)
# n.ahead = H - 1: the package sums h = 0..n.ahead, DY (2012) sum h = 0..H-1
# no.corr = FALSE: keep the off-diagonal elements of Sigma (correlated shocks), as in DY (2012)

# Spillover table
generate_spillover_table <- function(sp_full) {
  theta_percent <- sp_full * 100        # rows sum to 100
  dimnames(theta_percent) <- list(colnames(vol), colnames(vol))  # bank names on rows and columns
  
  from_others <- rowSums(theta_percent) - diag(theta_percent)   # per row    (receiver)
  to_others   <- colSums(theta_percent) - diag(theta_percent)   # per column (transmitter)
  net         <- to_others - from_others
  total       <- sum(from_others) / ncol(theta_percent)          # total spillover index S
  
  dy_tab <- cbind(theta_percent, FROM_others = from_others)      # add column
  dy_tab <- rbind(dy_tab,
                  TO_others = c(to_others, total),               # add row (+ S in the corner)
                  NET       = c(net, NA))
  return(round(dy_tab, 2))
}
spillover_table <- generate_spillover_table(sp_full$tables[[1]])
spillover_table



# askChatgpt: Can you compute the pairwise spillovers from the spillover_table and visualize them?
pairwise_matrix <- function(spillover_table) {
  banks <- names(vol)
  theta_tab <- spillover_table[banks, banks]      # the N x N block of shares (in %)
  pw   <- (t(theta_tab) - theta_tab)/length(banks)
  return(round(pw, 2))
}

pw_full <- pairwise_matrix(spillover_table)
pw_full

# Net spillovers per bank, from the NET row of the table
net_spill <- spillover_table["NET", names(vol)]
lim <- c(-1, 1) * ceiling(max(abs(net_spill)))      # symmetric axis: -10 to 10
barplot(sort(net_spill), las = 2, col = ifelse(sort(net_spill) > 0, "tomato", "skyblue"),
        ylim = lim, yaxt = "n",
        main = "Net volatility spillovers, full sample",
        ylab = "Net spillover (TO - FROM, %-points)")
axis(2, at = seq(lim[1], lim[2], by = 2), las = 1)  # ticks every 2 points, axis covers all bars
abline(h = 0)

# Network plot: arrow i -> j if i is a net transmitter to j
plot_spillover_network <- function(pw, main = "", max_w = max(pw)) {
  N   <- nrow(pw)
  ang <- pi / 2 - 2 * pi * (0:(N - 1)) / N # nodes on a circle, first one on top
  xy  <- cbind(cos(ang), sin(ang))
  r   <- 0.16                              # node radius (in plot units)
  plot(NA, xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35), asp = 1,
       axes = FALSE, xlab = "", ylab = "", main = main)
  for (i in 1:N) for (j in 1:N) {
    if (pw[i, j] > 0) {
      w     <- pw[i, j] / max_w                         # relative link strength (0, 1]
      u     <- (xy[j, ] - xy[i, ]) / sqrt(sum((xy[j, ] - xy[i, ])^2)) # unit direction i -> j
      nrm   <- c(-u[2], u[1])                           # perpendicular direction
      start <- xy[i, ] + (r + 0.02) * u                 # leave node i at its edge
      tip   <- xy[j, ] - (r + 0.02) * u                 # stop just outside node j
      base  <- tip - 0.10 * u                           # arrowhead length 0.10
      hw    <- 0.04 + 0.03 * w                          # arrowhead half-width
      segments(start[1], start[2], base[1], base[2], lwd = 1 + 5 * w, col = "grey15")
      polygon(c(tip[1], base[1] + hw * nrm[1], base[1] - hw * nrm[1]),  # solid arrowhead
              c(tip[2], base[2] + hw * nrm[2], base[2] - hw * nrm[2]),
              col = "grey15", border = NA)
    }
  }
  net_i <- rowSums(pw) # equals the net spillover of each bank
  symbols(xy[, 1], xy[, 2], circles = rep(r, N), inches = FALSE, add = TRUE,
          bg = ifelse(net_i > 0, "tomato", "skyblue"), fg = "grey15") # nodes drawn on top
  text(xy, labels = rownames(pw), cex = 0.8, font = 2)
  legend("bottomright", legend = c("net transmitter", "net receiver"), pch = 21,
         pt.bg = c("tomato", "skyblue"), pt.cex = 1.5, bty = "n", cex = 0.7)
}
plot_spillover_network(pw_full, main = "Pairwise net spillovers, full sample")


# --- Rolling-window total spillover index ---
# askChatgpt: Perform the rolling window spillover exercise according to the paper (DY (2012))

# spilloverRollingDY12() moves a window of 200 days through the data, one day at a time.
# For every window it estimates the VAR with func_est (here vars::VAR, with the
# arguments in params_est) and computes the DY (2012) spillover table with spilloverDY12().
#   data:       we pass the xts object 'vol' (not the matrix), so the package attaches
#               the date of the last day of every window to the results
#   n.ahead:    H - 1, because the package sums h = 0..n.ahead (see the top of this script)
#   no.corr:    FALSE, keep the correlation of the shocks (as in DY 2012). off-diagonal not zero
#   func_est:   name of the estimation function, as a string
#   params_est: its arguments: same lag order p as the full sample, with an intercept
roll_sp <- spilloverRollingDY12(data = vol, n.ahead = H - 1, no.corr = FALSE,
                                func_est = "VAR",
                                params_est = list(p = p_var, type = "const"),
                                window = window_size)

# overall() and net() on the rolling result return a list with one zoo time series
# (dates = last day of each window); [[1]] takes that series
roll_total <- overall(roll_sp)[[1]] # total spillover index per window
roll_net   <- net(roll_sp)[[1]]     # net spillover per bank (one column per bank) per window
roll_dates <- as.Date(index(roll_total))
summary(as.numeric(roll_total))

plot(roll_dates, as.numeric(roll_total), type = "n",
     xlab = "", ylab = "Total spillover index (%)",
     main = paste0("Rolling total volatility spillover (", window_size, "-day window, H = ", H, ")"))
shade_crises()
lines(roll_dates, as.numeric(roll_total), lwd = 1.2)
legend("bottomleft", legend = crisis_periods$name, fill = crisis_colors,
       border = NA, bty = "n", cex = 0.7)

plot(roll_dates, as.numeric(roll_net[, "BAC"]), type = "n",
     xlab = "", ylab = "Net spillover BAC (%)", main = "Rolling net spillover of BAC")
shade_crises()
lines(roll_dates, as.numeric(roll_net[, "BAC"]), lwd = 1.2)
abline(h = 0, lty = 2)
legend("bottomleft", legend = crisis_periods$name, fill = crisis_colors,
       border = NA, bty = "n", cex = 0.7)

# --- Subsample analysis: crisis vs non-crisis ---

# askChatgpt: Perform a subsample analysis using the crisis periods from script 00, and a calm period from 2013 and 2019.

# Crisis periods: ALL crisis windows defined in 00_Data_Extraction.R (crisis_periods):
#   GFC, Euro Debt Crisis, COVID-19 Crash, US-Iran War
# Calm periods: ALL stretches of the sample between these crises, each as its own subsample.
# (We do not pool all calm days into one sample: the VAR lags would then jump over the
#  crisis gaps, e.g. treating the last day before a crisis as "yesterday" of the first day after it.)
subsamples <- list("Full sample" = c(start(vol), end(vol)))
for (i in seq_len(nrow(crisis_periods))) {
  subsamples[[crisis_periods$name[i]]] <- c(crisis_periods$start[i], crisis_periods$end[i])
}

crises     <- crisis_periods[order(crisis_periods$start), ] # crises in chronological order
calm_start <- c(start(vol), crises$end + 1)                 # day after each crisis (and the sample start)
calm_end   <- c(crises$start - 1, end(vol))                 # day before each crisis (and the sample end)
for (i in which(calm_end > calm_start)) {                   # skip empty gaps (last crisis runs to the end)
  calm_name <- paste("Calm", format(calm_start[i], "%Y-%m"), "to", format(calm_end[i], "%Y-%m"))
  subsamples[[calm_name]] <- c(calm_start[i], calm_end[i])
}
names(subsamples) # full sample, 4 crises, calm periods in between

sub_data <- lapply(subsamples, function(d) coredata(vol)[index(vol) >= d[1] & index(vol) <= d[2], ])

# Same p and H in every subsample, so differences come from the data, not the specification.
# Note: short crises (COVID-19 and US-Iran War: only ~100-140 trading days) give noisy VAR
# estimates -> compare n_obs with the number of parameters per VAR equation (1 + N * p).
sub_spill <- lapply(sub_data, function(y) {
  spilloverDY12(VAR(y, p = p_var, type = "const"), n.ahead = H - 1, no.corr = FALSE)
})

sub_summary <- data.frame(
  n_obs         = sapply(sub_data, nrow),
  params_per_eq = 1 + ncol(vol) * p_var,
  total         = sapply(sub_spill, function(s) overall(s)[[1]]),
  BAC_to        = sapply(sub_spill, function(s) to(s)[[1]][["BAC"]]),
  BAC_from      = sapply(sub_spill, function(s) from(s)[[1]][["BAC"]]),
  BAC_net       = sapply(sub_spill, function(s) net(s)[[1]][["BAC"]])
)
round(sub_summary, 2)

round(sapply(sub_spill, function(s) net(s)[[1]]), 2) # net spillover per bank (rows) and period (columns)

# Total spillover index per period: crises (red) vs calm (blue) vs full sample (grey)
period_type <- ifelse(names(subsamples) == "Full sample", "grey",
                      ifelse(startsWith(names(subsamples), "Calm"), "skyblue", "tomato"))
barplot(sub_summary$total, names.arg = rownames(sub_summary), col = period_type, las = 2,
        cex.names = 0.7, ylab = "Total spillover index (%)", main = "Total spillover index per period")

# Full spillover tables for every crisis and calm period
periods <- setdiff(names(subsamples), "Full sample")
for (nm in periods) {
  s <- sub_spill[[nm]]$tables[[1]]
  cat("\n", nm, "\n")
  print(generate_spillover_table(s))
}

# Network plots for every crisis and calm period, with the same arrow scale in all panels
pw_sub <- lapply(sub_spill[periods],
                 function(s) pairwise_matrix(generate_spillover_table(s$tables[[1]])))
max_w <- max(sapply(pw_sub, max)) # largest link over all periods -> same arrow scale in every panel

par(mfrow = c(1, 3)) # three panels next to each other
for (nm in c("GFC", "COVID-19 Crash", "Calm 2020-07 to 2026-02")) {
  plot_spillover_network(pw_sub[[nm]], main = nm, max_w = max_w)
}  
par(mfrow = c(1, 1))