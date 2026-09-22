#DISCLAIMER:
#I use chatgpt as my guidance on coding debugs, knowledge clarification and expansion,and paper recommendations

install.packages("rugarch")

library(rugarch)
log_ret_clean <- 100 * diff(log(bac$Adjusted))

#Plot ACF of log return and absolute log return to illustrate why arma do not work
# ACF plot of log return
acf(log_ret_clean,
    main = "ACF of Log Returns",
    lag.max = 30)
# ACF of absolute log returns
acf(abs(log_ret_clean),
    main = "ACF of Absolute Log Returns",
    lag.max = 30)


#I use GARCH(1,1) as baseline model

# Ask for chatgpt about how to use rugarch package and the meaning of different specifications
#GARCH(1, 1) 
spec_garch_norm <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "norm"
)
fit_garch_norm <- ugarchfit(
  spec = spec_garch_norm,
  data = log_ret_clean,
  solver = "hybrid"
)

#Since financial returns usually have fat tails, Bollerslev(1987) proposed conditionally Student-t errors precisely to accommodate the heavy-tailed behavior; 
#So I will set the distribution.model = "std"

#Bollerslev, T. (1987). A Conditionally Heteroskedastic Time Series Model for Speculative Prices and Rates of Return. The Review of Economics and Statistics, 69, 542-547.
#https://doi.org/10.2307/1925546 

#GARCH(1, 1) with t
spec_garch_t <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

fit_garch_t <- ugarchfit(
  spec = spec_garch_t,
  data = log_ret_clean,
  solver = "hybrid"
)

coef(fit_garch_t)
# we can see that the parameter shape is 5.273 which is an indication of fat-tailed
#So I will use innovation distribution as student-t in the following models

#ARCH(1)
spec_arch1 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 0)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

fit_arch1 <- ugarchfit(
  spec = spec_arch1,
  data = log_ret_clean,
  solver = "hybrid"
)

#ARCH(2)
spec_arch2 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(2, 0)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

fit_arch2 <- ugarchfit(
  spec = spec_arch2,
  data = log_ret_clean,
  solver = "hybrid"
)

#GARCH(1,2)
spec_garch12 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 2)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

fit_garch12 <- ugarchfit(
  spec = spec_garch12,
  data = log_ret_clean,
  solver = "hybrid"
)

#Since the market's reaction to positive and negative news may be asymmetrical in terms of volatility (Engle and Ng, 1993)
#I also introduce two major asymmetrical GARCH models 
#ENGLE, R.F. and NG, V.K. (1993), Measuring and Testing the Impact of News on Volatility. The Journal of Finance, 48: 1749-1778. https://doi.org/10.1111/j.1540-6261.1993.tb05127.x

#GJR-GARCH
spec_gjr <- ugarchspec(
  variance.model = list(
    model = "gjrGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

fit_gjr <- ugarchfit(
  spec = spec_gjr,
  data = log_ret_clean,
  solver = "hybrid"
)

#EGARCH
spec_egarch <- ugarchspec(
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

fit_egarch <- ugarchfit(
  spec = spec_egarch,
  data = log_ret_clean,
  solver = "hybrid"
)

#check if asymmetry is statistical significant
fit_gjr@fit$matcoef
fit_egarch@fit$matcoef
#we can see from the result both with p-value of alpha1 smaller than 0.05, 
#so it is statistical significant that positive and negative shocks have different volatility effect

#Ask for chatgpt on how to extract AIC and BIC 
#Compare information criteria
models <- list(
  "ARCH(1)"      = fit_arch1,
  "ARCH(2"      = fit_arch2,
  "GARCH(1,1)-N"  = fit_garch_norm,
  "GARCH(1,1)"  = fit_garch_t,
  "GARCH(1,2)"  = fit_garch12,
  "GJR-GARCH(1,1)"  = fit_gjr,
  "EGARCH(1,1)"  = fit_egarch
)

IC <- t(sapply(models, function(x) {
  infocriteria(x)[1:2]
}))

colnames(IC) <- c("AIC", "BIC")

print(IC)
#We can see from the result, EGARCH has the lowest AIC and BIC, and also because of the nature of financial returns which has asymmetric volatility, so select EGARCH
coef(fit_egarch)
