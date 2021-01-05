#**********************************************************************************************
# Autoregressive Distributed Model
# By: JS Consulting
# Objectives: Time Series Regression
# Granger Causality Approach
#**********************************************************************************************

#**********************************************************************************************
# 1. SET WORKING DIRECTORY, LOAD DATA AND PACKAGES

## Set the working directory for the tutorial file
setwd("~/Desktop/Forecasting Model")

## Load preliminary packages
library(fpp2)

library(vars)
library(tseries)
library(TSstudio)
library(urca)
library(forecast)
library(tidyverse)

library(AER)
library(stargazer)

##Load dataset
mydata <- CPO_Prices_Data5
head(mydata)
names(mydata)
view(mydata)

## Dimension of the dataset
# 132 observations (11 years, 12 months)
# 14 variables: date, FCPO, SO, CO, RO, SFO, USD/CNY, USD/PKR, USD/MYR, USD/INR, USD/EUR, USD FUTURES, Mstock, Mexport
dim(mydata)

## Declare this as time series data
CPO <- ts(mydata[,2], start=c(2009,1), end=c(2019,12), frequency = 12)

# Format the date column
mydata$Date <- as.Date(mydata$Date, format = "%d.%m.%Y")

# Define CPO as an object
mydata$CPO <- ts(mydata$`FCPO Prices`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define SO as an object
mydata$SO <- ts(mydata$`SO Prices`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define SFO as an object 
mydata$SFO <- ts(mydata$`SFO Prices`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define RO as an object
mydata$RO <- ts(mydata$`RO Prices`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define USD/INR as an object
mydata$INR <- ts(mydata$`USD/INR`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define USD/CNY as an object
mydata$CNY <- ts(mydata$`USD/CNY`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define USD/EUR as an object
mydata$EUR <- ts(mydata$`USD/EUR`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define USDX as an object
mydata$USDX <- ts(mydata$`USD FUTURES`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define Mstock as an object
mydata$Mstock <- ts(mydata$`Mstock`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define Mexport as an object
mydata$Mexport <- ts(mydata$`Mexport`, start = c(2009,1), end = c(2019,12), frequency = 12)

#**********************************************************************************************
# 2. GRAPHING VARIABLES & TESTING AND ADJUSTING FOR STATIONARITY

# Time series plots for each variable
ts.plot(mydata$CPO, main = "CPO Prices")
ts.plot(mydata$SO, main = "SO Prices")
ts.plot(mydata$SFO, main = "SFO Prices")
ts.plot(mydata$RO, main = "RO Prices")
ts.plot(mydata$INR, main = "INR Prices")
ts.plot(mydata$CNY, main = "CNY Prices")
ts.plot(mydata$EUR, main = "EUR Prices")
ts.plot(mydata$USDX, main = "USDX Prices")
ts.plot(mydata$Mstock, main = "Mstock Prices")
ts.plot(mydata$Mexport , main = "Mexport Prices")

# Testing for stationarity (ADF Test)
adf.test(mydata$CPO) #Non-Stationary
adf.test(mydata$SO) #Non-Stationary
adf.test(mydata$SFO) #Non-Stationary
adf.test(mydata$RO) #Non-Stationary
adf.test(mydata$INR) #Non-Stationary
adf.test(mydata$CNY) #Non-Stationary
adf.test(mydata$EUR) #Non-Stationary
adf.test(mydata$USDX) #Non-Stationary
adf.test(mydata$Mstock) #Stationary
adf.test(mydata$Mexport) #Stationary

# Make variables stationary
dCPO <- diff(mydata$CPO) 
dSO  <- diff(mydata$SO) 
dSFO <- diff(mydata$SFO)
dRO <- diff(mydata$RO)
dINR <- diff(mydata$INR) 
dCNY <-diff(mydata$CNY) 
dEUR <-diff(mydata$EUR)
dUSDX <-diff(mydata$USDX)
dMstock <- diff(mydata$Mstock)
dMexport <- diff(mydata$Mexport)

ts.plot(dCPO)
ts.plot(dSO)
ts.plot(dSFO)
ts.plot(dRO)
ts.plot(dINR)
ts.plot(dCNY)
ts.plot(dEUR)
ts.plot(dUSDX)
ts.plot(dMstock)
ts.plot(dMexport)

# Re-test for Stationarity (ADF Test)
adf.test(dCPO) #Stationary 
adf.test(dSO) #Stationary 
adf.test(dSFO) #Stationary 
adf.test(dRO) #Stationary 
adf.test(dINR) #Stationary 
adf.test(dCNY) #Stationary 
adf.test(dEUR) #Stationary 
adf.test(dUSDX) #Stationary 
adf.test(dMstock) #Stationary 
adf.test(dMexport) #Stationary 


#**********************************************************************************************
# 3. VARIABLE CREATION: LAGS, DIFFERENCES

# Re-scale Mstock and Mexport variable in terms of 1000000's
mydata$Mstock=mydata$Mstock/1000000
mydata$Mexport=mydata$Mexport/1000000

view(mydata)

# A. CONSTRUCTING LAGS OF VARIABLES 

# First and second lag of CPO
CPO_lag1=c(NA,head(dCPO, -1))      # Shifts the dCPO variable forward 1 places, fills in first data point with NA
CPO_lag2=c(NA,NA,head(dCPO, -2))   # Shifts the dCPO variable forward 2 places, fills in first data point with NA

# First and second lag of SO
SO_lag1=c(NA,head(dSO, -1))      
SO_lag2=c(NA,NA,head(dSO, -2))   

# First and second lag of INR
INR_lag1=c(NA,head(dINR, -1))      
INR_lag2=c(NA,NA,head(dINR, -2))   

# First and second lag of EUR
EUR_lag1=c(NA,head(dEUR, -1))      
EUR_lag2=c(NA,NA,head(dEUR, -2))   

# First and second lag of USDX
USDX_lag1=c(NA,head(dUSDX, -1))      
USDX_lag2=c(NA,NA,head(dUSDX, -2))   

# First and second lag of Mstock
Mstock_lag1=c(NA,head(dMstock, -1))      
Mstock_lag2=c(NA,NA,head(dMstock, -2))   

# First and second lag of Mexport
Mexport_lag1=c(NA,head(dMexport, -1))      
Mexport_lag2=c(NA,NA,head(dMexport, -2)) 


#**********************************************************************************************
# 4. AUTOCORRELATIONS: acf(), diff()

# A. AUTOCORRELATIONS IN LEVELS
acf(mydata$CPO)
acf(mydata$SO)
acf(mydata$INR) 
acf(mydata$EUR) 
acf(mydata$USDX) 
acf(mydata$Mstock)
acf(mydata$Mexport)


# B. AUTOCORRELATIONS IN FIRST DIFFERENCES
acf(dCPO)
acf(dSO)
acf(dINR) 
acf(dEUR)
acf(dUSDX)
acf(dMstock)
acf(dMexport)

#**********************************************************************************************
# 4. GRANGER CAUSALITY

# Raw Data Granger Causality Tests
grangertest(mydata$CPO ~ mydata$SO, order = 5) #GC @ 10%
grangertest(mydata$CPO ~ mydata$CO, order = 5)
grangertest(mydata$CPO ~ mydata$RO, order = 5)
grangertest(mydata$CPO ~ mydata$SFO, order = 5)
grangertest(mydata$CPO ~ mydata$CNY, order = 5)
grangertest(mydata$CPO ~ mydata$INR, order = 5) #GC @ 5%
grangertest(mydata$CPO ~ PKR, order = 5)
grangertest(mydata$CPO ~ EUR, order = 5)
grangertest(mydata$CPO ~ mydata$MYR, order = 5)
grangertest(mydata$CPO ~ USD, order = 5)
grangertest(mydata$CPO ~ mydata$Mstock, order = 5) #GC @ 1%
grangertest(mydata$CPO ~ mydata$Mexport, order = 5) #GC @ 0.1%
            
# Differenced Data Granger Causality Tests
grangertest(dCPO ~ dSO, order = 5)
grangertest(dCPO ~ dCO, order = 5)
grangertest(dCPO ~ dRO, order = 5)
grangertest(dCPO ~ dSFO, order = 5)
grangertest(dCPO ~ dCNY, order = 5)
grangertest(dCPO ~ dINR, order = 5) #GC @ 5%
grangertest(dCPO ~ dPKR, order = 5)
grangertest(dCPO ~ dEUR, order = 5) #GC @ 5%
grangertest(dCPO ~ dMYR, order = 5)
grangertest(dCPO ~ dUSD, order = 5) #GC @ 5%
grangertest(dCPO ~ dMstock, order = 5) #GC @ 1%
grangertest(dCPO ~ dMexport, order = 5) #GC @ 1%


# Therefore, variables to look into are SO, INR, EUR, USD, Mstock and Mexport
#**********************************************************************************************
# 5. AUTOREGRESSIONS

# Number of observations in the time series (used below)
T=131 #no. of observations in the differenced time series 

# A. ESTIMATE AR(p) MODELS AND COMPUTE BIC/AIC

# AR(2) model with CPO
reg1=lm(dCPO~CPO_lag1+CPO_lag2,data=mydata)
cov1=vcovHC(reg1, type = "HC1")    
se1=sqrt(diag(cov1))
coeftest(reg1, vcov = vcovHC(reg1, "HC1"))
K1=3                             # Number of parameters in regression reg1
ssr1=sum(residuals(reg1)^2)      # SSR from the regression reg1
BIC1=log(ssr1/T)+K1*(log(T)/T)   # BIC from the regression reg1
AIC1=log(ssr1/T)+K1*(log(2)/T)   # AIC from the regression reg1

AIC1 #AIC1 = 7.564
BIC1 #AIC1 = 7.66
AIC(reg1) #AIC1 = 1349.82

# B. ESTIMATE ADL(p,q) MODELS

# ADL(2,2) model with CPO and SO
reg2=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2,data=mydata)
cov2=vcovHC(reg1, type = "HC1")    
se2=sqrt(diag(cov2))
coeftest(reg2, vcov = vcovHC(reg2, "HC1"))
K2=5                        
ssr2=sum(residuals(reg2)^2)     
BIC2=log(ssr2/T)+K2*(log(T)/T)
AIC2=log(ssr2/T)+K2*(log(2)/T)

AIC2 #AIC2 = 7.556
BIC2 #BIC2 = 7.716
AIC(reg2) #AIC2 = 1351.368

# C.ESTIMATE ADL(p,q1,q2) AND ADL(p,q1,q2,q3,q4) MODELS

# ADL(2,2,2) model with CPO, SO and INR
reg3=lm(dCPO~CPO_lag1+CPO_lag2++SO_lag1+SO_lag2+INR_lag1+INR_lag2,data=mydata)
cov3=vcovHC(reg3, type = "HC1")    
se3=sqrt(diag(cov3))
coeftest(reg3, vcov = vcovHC(reg3, "HC1"))
K3=7                             
ssr3=sum(residuals(reg3)^2)     
BIC3=log(ssr3/T)+K3*(log(T)/T)
AIC3=log(ssr3/T)+K3*(log(2)/T)

AIC3 # AIC = 7.543
BIC3 # BIC = 7.766
AIC(reg3) #AIC4 = 1352.30

# ADL(2,2,2,2) model with CPO, SO, INR and EUR
reg4=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+INR_lag1+INR_lag2+EUR_lag1+EUR_lag2,data=mydata)
cov4=vcovHC(reg4, type = "HC1")    
se4=sqrt(diag(cov4))
coeftest(reg4, vcov = vcovHC(reg4, "HC1"))
K4=9                             
ssr4=sum(residuals(reg4)^2)     
BIC4=log(ssr4/T)+K4*(log(T)/T)
AIC4=log(ssr4/T)+K4*(log(2)/T)

AIC4 # AIC4 = 7.522
BIC4 # BIC4 = 7.809
AIC(reg4) #AIC4 = 1352.215


# ADL(2,2,2,2,2) model with CPO, SO, INR, EUR and USDX
reg5=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+INR_lag1+INR_lag2+EUR_lag1+EUR_lag2+USDX_lag1+USDX_lag2,data=mydata)
cov5=vcovHC(reg5, type = "HC1")    
se5=sqrt(diag(cov5))
K5=11                            
ssr5=sum(residuals(reg5)^2)     
BIC5=log(ssr5/T)+K5*(log(T)/T)
AIC5=log(ssr5/T)+K5*(log(2)/T)

AIC5 # AIC5 = 7.519
BIC5 # BIC5 = 7.87
AIC(reg5) #AIC5 = 1354.503

# ADL(2,2,2,2,2,2) model with CPO, SO, INR, EUR, USDX and Mstock
reg6=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+INR_lag1+INR_lag2+EUR_lag1+EUR_lag2+USDX_lag1+USDX_lag2+Mstock_lag1+Mstock_lag2,data=mydata)
cov6=vcovHC(reg6, type = "HC1")    
se6=sqrt(diag(cov6))
K6=13                       
ssr6=sum(residuals(reg6)^2)     
BIC6=log(ssr6/T)+K6*(log(T)/T)  
AIC6=log(ssr6/T)+K6*(log(2)/T)  

AIC6 #AIC6 = 7.453
BIC6 #BIC6 = 7.868
AIC(reg6) #AIC6 = 1348.572

# ADL(2,2,2,2,2,2,2) model with CPO, SO, INR, EUR, USDX, Mstock and Mexport (THIS ONE)
reg7=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+INR_lag1+INR_lag2+EUR_lag1+EUR_lag2+USDX_lag1+USDX_lag2+Mstock_lag1+Mstock_lag2+Mexport_lag1+Mexport_lag2,data=mydata)
cov7=vcovHC(reg7, type = "HC1")    
se7=sqrt(diag(cov7))
K7=15                       
ssr7=sum(residuals(reg7)^2)     
BIC7=log(ssr7/T)+K7*(log(T)/T)  
AIC7=log(ssr7/T)+K7*(log(2)/T)  

AIC7 #AIC7 = 7.435
BIC7 #BIC7 = 7.913
AIC(reg7) #AIC7 = 1348.889

# ADL(2,2,2,2,2,2,2) model with CPO, INR, EUR, USDX, Mstock and Mexport
reg8=lm(dCPO~CPO_lag1+CPO_lag2+INR_lag1+INR_lag2+EUR_lag1+EUR_lag2+USDX_lag1+USDX_lag2+Mstock_lag1+Mstock_lag2+Mexport_lag1+Mexport_lag2,data=mydata)
cov8=vcovHC(reg8, type = "HC1")    
se8=sqrt(diag(cov8))
K8=13                       
ssr8=sum(residuals(reg8)^2)     
BIC8=log(ssr7/T)+K8*(log(T)/T)  
AIC8=log(ssr7/T)+K8*(log(2)/T)  

# ADL(1,1,1,1,1,1,1) model with CPO, SO, INR, EUR, USDX, Mstock and Mexport (1 LAG)
reg9=lm(dCPO~CPO_lag1+SO_lag1+INR_lag1+EUR_lag1+USDX_lag1+Mstock_lag1+Mexport_lag1,data=mydata)
cov9=vcovHC(reg9, type = "HC1")    
se9=sqrt(diag(cov9))
K9=8                       
ssr9=sum(residuals(reg9)^2)     
BIC9=log(ssr7/T)+K9*(log(T)/T)  
AIC9=log(ssr7/T)+K9*(log(2)/T)  

AIC9 #AIC9 = 7.398
BIC9 #BIC9 = 7.653
AIC(reg9) #AIC9 = 1359.623


## Regression output table
# Discuss the results from stargazer() as asked in the question on the assignment sheet.
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, type="text",
          se=list(se1, se2, se3, se4, se5, se6, se7),
          digits=3, dep.var.labels=c("CPO"),
          out="reg_output(GC).html")   # Output results to your directory in a text file

#**********************************************************************************************
# 6. FORECASTING

# A. RE-RUN ADL REGRESSION FOR FORECASTING
reg7=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+INR_lag1+INR_lag2+EUR_lag1+EUR_lag2+USDX_lag1+USDX_lag2+Mstock_lag1+Mstock_lag2+Mexport_lag1+Mexport_lag2,data=mydata)
coeftest(reg7, vcov = vcovHC(reg7, "HC1"))

# B. WITHIN-SAMPLE FORECASTING

## Use dataset and predict() function to generate in-sample predictions based on reg5 regression model estimates
CPO_fcast_in=predict(reg7, data=mydata) 
CPO_fcast_in <- ts(CPO_fcast_in, start = c(2009,3), end = c(2019,12), frequency = 12)
ts.plot(CPO_fcast_in)
ts.plot(dCPO)
lines(CPO_fcast_in, col = "orange",type="l")

view(CPO_fcast_in)


view(dCPO)
# Add the within-sample forecasts and forecast errors t othe mydata dataset
# Need to make the first 2 observations in search_fcast_in "NA" because you cannot compute forecast error for first 2 observations
# due to the first and second lags of price in the regression model, which means the first and second observations
# do not have a within-sample prediction
CPO_fcast_in=c(NA,NA,CPO_fcast_in)   

# Add forecast errors to mydata
CPO_fcast_in_error=dCPO-CPO_fcast_in
plot(CPO_fcast_in_error)

## Output some within-sample forecasting results
sprintf("Within-sample forecast for period T=130: %f", CPO_fcast_in[T])
sprintf("Within-sample forecast error for period T=132: %f", CPO_fcast_in_error[T])
sprintf("Average forecast error: %f", mean(CPO_fcast_in_error,na.rm=TRUE))         # Compute mean without NA data points
sprintf("Std. Dev. of forecast error: %f", sd(CPO_fcast_in_error,na.rm=TRUE))      # Compute std. dev. without NA data points

## Plot Search Data and Within-Sample Forecasts
plot(mydata$Date,mydata$CPO_fcast_in, type="l",col="blue")


## Plot Forecast Errors

plot(mydata$Date, CPO_fcast_in_error, type="l",col="green")

# C. OUT-OF-SAMPLE FORECASTING

T=131

## Data frame for the last observation in the sample of T=131 (defined on line 70) for generating a forecast
newdata1=data.frame(CPO_lag1=CPO_lag1[T], CPO_lag2=CPO_lag2[T],SO_lag1=SO_lag1[T],SO_lag2=SO_lag2[T], INR_lag1=INR_lag1[T],INR_lag2=INR_lag2[T],
                    Mstock_lag1=Mstock_lag1[T],Mstock_lag2=Mstock_lag2[T], Mexport_lag1=Mexport_lag1[T],Mexport_lag2=Mexport_lag2[T])

view(newdata1)
## Use the predict() command to forecast out the next observation in period T+1 out of the sample
CPO_fcast_out=predict(reg7, newdata=newdata1) 

## Standard error of search_fcast_in_error=RMSFE (see slides 27 and 28 in Lecture Note 9)
RMSFE=sqrt(var(residuals(reg7)))

## 95% CI for out-of-sample forecast
CPO_fcast_out_CIL=CPO_fcast_out-1.96*RMSFE
CPO_fcast_out_CIH=CPO_fcast_out+1.96*RMSFE

## Output results
sprintf("Out-of-sample forecast for period T=132: %f", CPO_fcast_out)
sprintf("95 CI lower bound for out-of-sample forecast: %f", CPO_fcast_out_CIL)
sprintf("95 CI upper bound for out-of-sample forecast: %f", CPO_fcast_out_CIH)

CPO_fcast_out

#**********************************************************************************************
# 7. FORECASTING WITH VAR

library(vars)
VAR_ALL_data <- window(ts.union(dCPO, dSO, dEUR, dINR, dUSDX, dMstock, dMexport), start = c(2009,2), end = c(2019,12)) 
VAR_ALL <- VAR(y = VAR_ALL_data, p = 2)
VAR_ALL
summary(VAR_ALL)

var_fit <- fitted(VAR_ALL)
var_fit_ts <- ts(var_fit, start = c(2009,3), end = c(2019,12), frequency = 12)
ts.plot(var_fit_ts[,1])

fitted_ADL <- fitted(reg7)
fitted_ADL_ts <- ts(fitted_ADL, start = c(2009,3), end = c(2019,12), frequency = 12)
ts.plot(var_fit_ts[,1])
lines(fitted_ADL_ts, col = "blue", type = "l")

fcst <- predict(VAR_ALL)
view(fcst) 
plot(fcst)
view(forecasts_ALL$dCPO)

#**********************************************************************************************
# 8. DIAGNOSTICS

# Co-Intergration (Johnason Testing - Trace)
# https://stats.stackexchange.com/questions/107468/cointegration-same-thing-as-stationary-residuals
v1 <- cbind(dCPO, dSO, dINR, dEUR, dUSDX, dMstock, dMexport)
ctest1 <- ca.jo(v1, type = "trace", ecdet = "const") 
summary(ctest1)

v2 <- cbind(mydata$CPO, mydata$SO, mydata$INR, mydata$EUR, mydata$USDX, mydata$Mstock, mydata$Mexport)
ctest1 <- ca.jo(v2, type = "trace", ecdet = "const", K = 9) 
summary(ctest1)

ur.df(window(CPO, c(2009, 1), c(2019, 12)), lags = 2, selectlags = "AIC", type = "trend") 

# Serial Correlation
Serial1 <- serial.test(VAR_ALL, lags.pt = 12, type = "PT.asymptotic")
Serial1 #do not reject null (p>a), no serial correlation, test passed

# Heteroskedasticity 
Arch1 <- arch.test(VAR_ALL, lags.multi = 12, multivariate.only = TRUE)
Arch1 #do not reject null (p>a), no heteroskedasticity, test passed

# Normal Distribution 
Norm1 <- normality.test(VAR_ALL, multivariate.only = TRUE)
Norm1 #reject the null hypotheses, residuals are not normally distributed

# Testing for Structural Breaks in the Residuals 
Stability1 <- stability(VAR_ALL, type = "OLS-CUSUM")
plot(Stability1) #no points exceed CIs, system is stable 

