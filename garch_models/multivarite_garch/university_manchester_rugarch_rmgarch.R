#### University of Manchester Tutorial ####

library(tidyverse)
library(quantmod)
library(rugarch)
# library(rmgarch)

startDate = as.Date("2007-01-03") #Specify period of time we are interested in
endDate = as.Date("2018-04-30")

# 1. Preliminaries ----

# Retrieving stock information ----

# IBM
getSymbols("IBM", from = startDate, to = endDate)
# Google
getSymbols("GOOG", from = startDate, to = endDate)
# British Petroleum 
getSymbols("BP", from = startDate, to = endDate)

head(IBM)
str(IBM)

chartSeries(GOOG)

# Creating returns ----

# There are two ways to create returns in quantmod: 
## dailyReturn: Creates daily returns
## weeklyReturn: Creates weekly returns

# Daily returns (xts objects)
rIBM = dailyReturn(IBM)
rGOOG = dailyReturn(GOOG)
rBP = dailyReturn(BP)

# The xts objects are transformed straight away into numerical variables when
# they are use as columns of a data.frame 
rX = data.frame(rIBM, rGOOG, rBP) 
names(rX) = c("rIBM", "rBP", "rGOOOG")

# 2. Univariate GARCH model ----

# 2.1 Specifying and estimating the model ----

# Default specification for ugarchspec: ARMA(1,1)-sGARCH(1,1) with normal errors
ug_spec = ugarchspec()
ug_spec

# Estimating the model
ugfit = ugarchfit(spec = ug_spec, data = rIBM)
ugfit

# 2.2 Elements contain in the ugarchfit object ----

# ugarchfit objects contain two slots: 
## @model
## @fit

# Elements in the @model slot
names(ugfit@model)

# Elements in the @fit slot
names(ugfit@fit)

# Extracting the estimating coefficients, conditional variances and squared residuals

ugfit@fit$coef
ug_var <- ugfit@fit$var   # save the estimated conditional variances
ug_res2 <- (ugfit@fit$residuals)^2   # save the estimated squared residuals

# Plot the squared residuals and the estimated conditional variance

plot(ug_res2, type = "l")
lines(ug_var, col = "green")

# 2.3 Model forecasting ----

# ugarchforecast objects contain two slots: 
## @model
## @forecast

ugfore <- ugarchforecast(ugfit, n.ahead = 10)
ugfore

# Elements in the @model slot
names(ugfore@model)

# Elements in the @forecast slot
names(ugfore@forecast)

# Plot forecast conditional volatility

ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type = "l")

# To put these forecasts into context let's display them together with the last 50 observations used in the estimation.

ug_var_t <- c(tail(ug_var,20),rep(NA,10))  # gets the last 20 observations
ug_res2_t <- c(tail(ug_res2,20),rep(NA,10))  # gets the last 20 observations
ug_f <- c(rep(NA,20),(ug_f)^2)

plot(ug_res2_t, type = "l")
lines(ug_f, col = "orange")
lines(ug_var_t, col = "green")