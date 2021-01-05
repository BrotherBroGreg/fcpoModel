########################################################
# Data Preparation !!!!CODE STARTS HERE !!!!!!!!!CODE STARTS HERE !!!!!
########################################################


# Load forecasting package, and other preliminary packages
library(fpp2)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(readxl)

# Load dataset
CPO_Prices_Data <- read_excel("CPO_Prices_Data.xlsx", sheet = "CPO Prices")
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
ts.plot(Mprod, margins=)

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

