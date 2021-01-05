rm(list=ls())

########################################################
# Data Preparation 
########################################################

rm(list=ls())

# Load forecasting package, and other preliminary packages
library(fpp2)

install.packages("vars")
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

# Load dataset
data <- CPO_Prices_Data
head(data)

# Declare this as time series data
CPO <- ts(data[,2], start=c(2009,1), end=c(2019,12), frequency = 12)

# Format the date column
CPO_Prices_Data$Date <- as.Date(CPO_Prices_Data$Date, format = "%d.%m.%Y") 

########################################################
# Defining Factors/Features as Time Series
########################################################

# Define CPO as an object
CPO <- ts(CPO_Prices_Data$`CPO Prices`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define SO as an object
SO <- ts(CPO_Prices_Data$`SO Prices`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define CO as an object
CO <- ts(CPO_Prices_Data$`CO Prices`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define myrcny as an object
CNY <- ts(CPO_Prices_Data$`myrcny`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define myrinr as an object
INR <- ts(CPO_Prices_Data$`myrinr`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define myreur as an object
EUR <- ts(CPO_Prices_Data$`myreur`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define myrusd as an object
USD <- ts(CPO_Prices_Data$`myrusd`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define Mprod as an object
Mprod <- ts(CPO_Prices_Data$`Mprod`, start = c(2009,1), end = c(2019,12), frequency = 12)

########################################################
# Graphing and Testing for Stationarity
########################################################

# Time series plots for each variable
ts.plot(CPO)
ts.plot(SO)
ts.plot(CO)
ts.plot(INR)
ts.plot(EUR)
ts.plot(USD)
ts.plot(Mprod)

# Testing for stationarity
pp.test(CPO) #Non-Stationary
pp.test(SO) #Non-Stationary
pp.test(CO) #Non-Stationary
pp.test(INR) #Non-Stationary
pp.test(EUR) #Non-Stationary
pp.test(USD) #Non-Stationary
pp.test(Mprod) #Stationary

########################################################
# Removing Seasonality and Trends
########################################################

# Remove Seasonality in Mprod
dMprod <- diff(Mprod, lag = 12)
ts.plot(dMprod)
d2Mprod <- diff(dMprod, lag = 12)
ts.plot(d2Mprod)

# Make other variables stationary
dCPO <- diff(CPO) #Stationary 
dSO  <- diff(SO) #Stationary
dCO  <- diff(CO) #Stationary
dINR <- diff(INR) #Stationary
dCNY <- diff(CNY) #Stationary
dEUR <- diff(EUR) #Stationary
dUSD <- diff(USD) #Stationary

adf.test(dCPO)
adf.test(dSO)
adf.test(dCO)
adf.test(dINR)
adf.test(dCNY)
adf.test(dEUR)
adf.test(dUSD)

########################################################
# Lag Selection
# Vector Autoregression (VAR) model
########################################################

OLS1 <- lm(CPO ~ SO + CO + INR + CNY + EUR + USD + Mprod, data = data)
summary(OLS1)

#VAR function 
v1 <- cbind(dCPO, dSO, dCO, dEUR, dINR, dCNY)
colnames(v1) <- cbind("CPO", "SO", "CO", "EUR", "INR", "CNY")

lagselect <- VARselect(v1, lag.max = 12, type = "const")
lagselect$selection

Model <- VAR(v1, p = 1, type = "const", season = NULL, exogen = NULL)
summary(Model)

# Forecast
forecasts <- predict(Model)
forecasts
plot(forecasts)

########################################################
# VAR Diagonstics
# https://www.youtube.com/watch?v=qyGlB4cqZ9Q&ab_channel=JustinEloriaga
########################################################

# Serial Correlation
Serial1 <- serial.test(Model, lags.pt = 12, type = "PT.asymptotic")
Serial1 #do not reject null (p>a), no serial correlation, test passed

# Heteroskedasticity 

Arch1 <- arch.test(Model, lags.multi = 12, multivariate.only = TRUE)
Arch1 #do not reject null (p>a), no heteroskedasticity, test passed


# Normal Distribution 
Norm1 <- normality.test(Model, multivariate.only = TRUE)
Norm1 #do not reject null, residuals are normally distributed, test passsed

# Testing for Structural Breaks in the Residuals 
Stability1 <- stability(Model, type = "OLS-CUSUM")
plot(Stability1) #no points exceed CIs, system is stable 

########################################################
# Other Notes
########################################################

# Run separate equations with OLS
library(dynlm)

eq1 <- dynlm(CPO ~ L(CPO, 1:2) + L(SO, 1:2), start = c(2009,1), end = c(2019,12))

eq2 <- dynlm(SO ~ L(CPO, 1:2) + L(SO, 1:2), start = c(2009,1), end = c(2019,12))


#rename coefficients
names(eq1$coefficients) <- c("Intercept", "CPO_t-1", "CPO_t-2", "SO_t-1", "SO_t-2")

names(eq2$coefficients) <- names(eq1$coefficients)


#robust coefficient summaries
library(sandwich)
library(lmtest)
coeftest(eq1, vcov. = sandwich)

# (no statistical significance, only for the first lag)


#CPO prices only
eq3 <- dynlm(CPO ~ L(CPO, 1), start = c(2009,1), end = c(2019,12))
coeftest(eq3, vcov. = sandwich)


#CPO and Indian exchange rate
IND <- ts(CPO_Prices$myrinr, start = c(2009,1), end = c(2019,12), frequency = 12)
eq4 <- dynlm(CPO ~ L(CPO, 1) + L(IND,1), start = c(2009,1), end = c(2019,12))
eq5 <- dynlm(IND ~ L(CPO, 1) + L(IND,1), start = c(2009,1), end = c(2019,12))
coeftest(eq4, vcov. = sandwich)
coeftest(eq5,vcov. = sandwich)


#VAR function
VAR_INR_data <- window(ts.union(CPO, INR), start = c(2009,1), end = c(2019,12)) 
library(vars)
VAR_INR <- VAR(y = VAR_INR_data, p = 1)
VAR_INR
summary(VAR_INR$varresult$CPO)$adj.r.squared
summary(VAR_INR$varresult$INR)$adj.r.squared


#Granger causality
linearHypothesis(eq1, hypothesis.matrix = c("SO_t-1", "SO_t-2"), vcov. = sandwich)

eq6 <- dynlm(CPO ~ L(CPO, 1:2) + L(IND, 1:2), start = c(2009,1), end = c(2019,12))
names(eq6$coefficients) <- c("Intercept", "CPO_t-1", "CPO_t-2", "IND_t-1", "IND_t-2")
eq7 <- dynlm(IND ~ L(CPO, 1:2) + L(IND, 1:2), start = c(2009,1), end = c(2019,12))
names(eq7$coefficients) <- names(eq6$coefficients)


linearHypothesis(eq6, hypothesis.matrix = c("IND_t-1", "IND_t-2"), vcov. = sandwich)
linearHypothesis(eq7, hypothesis.matrix = c("CPO_t-1", "CPO_t-2"), vcov. = sandwich)


#forecasts
forecasts_IND <- predict(VAR_IND)
forecasts_IND
plot(forecasts_IND)


#VAR function 2
VAR_ALL_data <- window(ts.union(CPO, SO, CO, EUR, INR, CNY, USD), start = c(2009,1), end = c(2019,12)) 
library(vars)
VAR_ALL <- VAR(y = VAR_ALL_data, p = 1)
VAR_ALL
summary(VAR_ALL)

forecasts_ALL <- predict(VAR_ALL)
forecasts_ALL
plot(forecasts_ALL)


fit <- fitted(VAR_IND)
ts.plot(fit) 
ts.plot(CPO)

