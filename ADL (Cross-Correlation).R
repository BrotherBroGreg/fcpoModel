#**********************************************************************************************
# Autoregressive Distributed Model
# By: JS Consulting
# Objectives: Time Series Regression
# Cross Correlation Approach (SO, RO, SFO, INR)
#**********************************************************************************************

#**********************************************************************************************
# 1. SET WORKING DIRECTORY, LOAD DATA AND PACKAGES

## Set the working directory for the tutorial file
setwd("~/Desktop/CPO Project")

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
library(readxl)
CPO_Prices_Data5 <- read_excel("~/Downloads/CPO_Prices_Data5.xlsx")
mydata <- CPO_Prices_Data5
head(mydata)
names(mydata)
view(mydata)

## Dimension of the dataset
# 132 observations (11 years, 12 months)
# 14 variables: Date, FCPO, SO, CO, RO, SFO, USD/CNY, USD/PKR, USD/MYR, USD/INR, USD/EUR, USD FUTURES, Mstock, Mexport
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

# Define MYR as an object
mydata$MYR <- ts(mydata$`USD/MYR`, start = c(2009,1), end = c(2019,12), frequency = 12)

# Define INR as an object
mydata$INR <- ts(mydata$`USD/INR`, start = c(2009,1), end = c(2019,12), frequency = 12)

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
ts.plot(mydata$MYR)
ts.plot(mydata$INR, main = "INR Prices")
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
adf.test(mydata$Mstock) #Stationary
adf.test(mydata$Mexport) #Stationary

# Make variables stationary
dCPO <- diff(mydata$CPO) 
dSO  <- diff(mydata$SO) 
dSFO <- diff(mydata$SFO)
dRO <- diff(mydata$RO)
dMYR <- diff(mydata$MYR)
dINR <- diff(mydata$INR) 
dUSDX <-diff(mydata$USDX) 
dMstock <- diff(mydata$Mstock)
dMexport <- diff(mydata$Mexport)


# Re-test for Stationarity (ADF Test)
adf.test(dCPO) #Stationary 
adf.test(dSO) #Stationary 
adf.test(dSFO) #Stationary 
adf.test(dRO) #Stationary 
adf.test(dMYR)
adf.test(dINR) #Stationary 
adf.test(dCNY) #Stationary 
adf.test(dMstock) #Stationary 
adf.test(dMexport) #Stationary 


#**********************************************************************************************
# 3. VARIABLE CREATION: LAGS, DIFFERENCES

view(mydata)

# A. CONSTRUCTING LAGS OF VARIABLES 

# First and second lag of CPO
CPO_lag1=c(NA,head(dCPO, -1))      # Shifts the dCPO variable forward 1 places, fills in first data point with NA
CPO_lag2=c(NA,NA,head(dCPO, -2))   # Shifts the dCPO variable forward 2 places, fills in first data point with NA


# First and second lag of SO
SO_lag1=c(NA,head(dSO, -1))      
SO_lag2=c(NA,NA,head(dSO, -2))  

# First and second lag of RO
RO_lag1=c(NA,head(dRO, -1))      
RO_lag2=c(NA,NA,head(dRO, -2))  

# First and second lag of SFO
SFO_lag1=c(NA,head(dSFO, -1))      
SFO_lag2=c(NA,NA,head(dSFO, -2))  

# First and second lag of MYR
MYR_lag1=c(NA,head(dMYR, -1))      
MYR_lag2=c(NA,NA,head(dMYR, -2))   

# First and second lag of INR
INR_lag1=c(NA,head(dINR, -1))      
INR_lag2=c(NA,NA,head(dINR, -2))   

# First and second lag of USDX
USDX_lag1=c(NA,head(dUSDX, -1))      
USDX_lag2=c(NA,NA,head(dUSDX, -2))   

#**********************************************************************************************
# 4. AUTOCORRELATIONS: acf(), diff()

# A. AUTOCORRELATIONS IN LEVELS
acf(mydata$CPO)
acf(mydata$SO)
acf(mydata$SFO)
acf(mydata$RO)
acf(mydata$INR) 
acf(mydata$USDX)

# B. AUTOCORRELATIONS IN FIRST DIFFERENCES
acf(dCPO)
acf(dSO)
acf(dSFO)
acf(dRO)
acf(dINR)
acf(dUSDX)


#**********************************************************************************************
# 5. AUTOREGRESSIONS

# Number of observations in the time series (used below)
T=131

# A. ESTIMATE AR(p) MODELS AND COMPUTE BIC/AIC

# AR(2) model with CPO
reg1=lm(dCPO~CPO_lag1+CPO_lag2,data=mydata)
cov1=vcovHC(reg1, type = "HC1")    
se1=sqrt(diag(cov1))
K1=3                             # Number of parameters in regression reg1
ssr1=sum(residuals(reg1)^2)      # SSR from the regression reg1
BIC1=log(ssr1/T)+K1*(log(T)/T)   # BIC from the regression reg1
AIC1=log(ssr1/T)+K1*(log(2)/T)   # AIC from the regression reg1

AIC1 #AIC1 = 7.564
AIC(reg1) #AIC1 = 1349.82

# AR(2,2) model with CPO and SO
reg2=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2,data=mydata)
cov2=vcovHC(reg1, type = "HC1")    
se2=sqrt(diag(cov2))
coeftest(reg2, vcov = vcovHC(reg2, "HC1"))
K2=5                        
ssr2=sum(residuals(reg2)^2)     
BIC2=log(ssr2/T)+K2*(log(T)/T)
AIC2=log(ssr2/T)+K2*(log(2)/T)

AIC2 #AIC2 = 7.556
AIC(reg2) #AIC2 = 1351.368

# AR(2,2,2) model with CPO, SO and RO
reg3=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+RO_lag1+RO_lag2,data=mydata)
cov3=vcovHC(reg3, type = "HC1")    
se3=sqrt(diag(cov3))
K3=7                             
ssr3=sum(residuals(reg3)^2)     
BIC3=log(ssr3/T)+K3*(log(T)/T)
AIC3=log(ssr3/T)+K3*(log(2)/T)

AIC3 #AIC3 = 7.554
AIC(reg3) #AIC3 = 1353.71

# AR(2,2,2,2) model with CPO, SO, RO and SFO
reg4=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+RO_lag1+RO_lag2+SFO_lag1+SFO_lag2,data=mydata)
cov4=vcovHC(reg4, type = "HC1")    
se4=sqrt(diag(cov4))
K4=9                             
ssr4=sum(residuals(reg4)^2)     
BIC4=log(ssr4/T)+K4*(log(T)/T)
AIC4=log(ssr4/T)+K4*(log(2)/T)

AIC4 #AIC4 = 7.563
AIC(reg4) #AIC4 = 1357.613

# ADL(2,2,2,2,2) model with CPO, SO, RO, SFO and INR
reg5=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+RO_lag1+RO_lag2+SFO_lag1+SFO_lag2+INR_lag1+INR_lag2,data=mydata)
cov5=vcovHC(reg5, type = "HC1")    
se5=sqrt(diag(cov5))
K5=11                            
ssr5=sum(residuals(reg5)^2)     
BIC5=log(ssr5/T)+K5*(log(T)/T)
AIC5=log(ssr5/T)+K5*(log(2)/T)

AIC5 # AIC5 = 7.548
AIC(reg5) #AIC5 = 1358.276

# ADL(2,2,2,2,2,2) model with CPO, SO, RO, SFO, INR and USDX
reg6=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+RO_lag1+RO_lag2+SFO_lag1+SFO_lag2+INR_lag1+INR_lag2+USDX_lag1+USDX_lag2,data=mydata)
cov6=vcovHC(reg6, type = "HC1")    
se6=sqrt(diag(cov6))
K6=13                       
ssr6=sum(residuals(reg6)^2)     
BIC6=log(ssr6/T)+K6*(log(T)/T)  
AIC6=log(ssr6/T)+K6*(log(2)/T)  

AIC6 #AIC6 = 7.525
AIC(reg6) #AIC6 = 1357.967


# ADL(2,2,2,2,2,2,2) model with CPO, SO, RO, SFO, INR, USDX and MYR
reg7=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+RO_lag1+RO_lag2+SFO_lag1+SFO_lag2+INR_lag1+INR_lag2+USDX_lag1+USDX_lag2+MYR_lag1+MYR_lag2,data=mydata)
cov7=vcovHC(reg7, type = "HC1")    
se7=sqrt(diag(cov7))
K7=15                       
ssr7=sum(residuals(reg7)^2)     
BIC7=log(ssr7/T)+K7*(log(T)/T)  
AIC7=log(ssr7/T)+K7*(log(2)/T)  

AIC7 #AIC7 = 7.533
AIC(reg7) #AIC7 = 1361.673

## Regression output table
# Discuss the results from stargazer() as asked in the question on the assignment sheet.
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, type="text",
          se=list(se1, se2, se3, se4, se5, se6, se7),
          digits=3, dep.var.labels=c("CPO"),
          out="reg_output(CC).html")   # Output results to your directory in a text file

#**********************************************************************************************
# 3. FORECASTING

# A. RE-RUN ADL REGRESSION FOR FORECASTING
reg7=lm(dCPO~CPO_lag1+CPO_lag2+SO_lag1+SO_lag2+RO_lag1+RO_lag2+SFO_lag1+SFO_lag2+INR_lag1+INR_lag2+USDX_lag1+USDX_lag2+MYR_lag1+MYR_lag2,data=mydata)
coeftest(reg7, vcov = vcovHC(reg7, "HC1"))

# B. WITHIN-SAMPLE FORECASTING

## Use dataset and predict() function to generate in-sample predictions based on reg5 regression model estimates
CPO_fcast_in=predict(reg7, data=mydata) 
view(CPO_fcast_in)
ts.plot(dCPO)
lines(CPO_fcast_in, col = "orange")
ts.plot(CPO_fcast_in)

# Add the within-sample forecasts and forecast errors t othe mydata dataset
# Need to make the first 2 observations in search_fcast_in "NA" because you cannot compute forecast error for first 2 observations
# due to the first and second lags of price in the regression model, which means the first and second observations
# do not have a within-sample prediction
CPO_fcast_in=c(NA,NA,CPO_fcast_in)   

# Add forecast errors to mydata
CPO_fcast_in_error=dCPO-CPO_fcast_in
plot(CPO_fcast_in_error)

## Output some within-sample forecasting results
sprintf("Within-sample forecast for period T=131: %f", CPO_fcast_in[T])
sprintf("Within-sample forecast error for period T=131: %f", CPO_fcast_in_error[T])
sprintf("Average forecast error: %f", mean(CPO_fcast_in_error,na.rm=TRUE))         
sprintf("Std. Dev. of forecast error: %f", sd(CPO_fcast_in_error,na.rm=TRUE))     

# C. OUT-OF-SAMPLE FORECASTING

T=131

## Data frame for the last observation in the sample of T=131 (defined on line 70) for generating a forecast
newdata1=data.frame(CPO_lag1=CPO_lag1[T], CPO_lag2=CPO_lag2[T],SO_lag1=SO_lag1[T],SO_lag2=SO_lag2[T], RO_lag1=RO_lag1[T],RO_lag2=RO_lag2[T],
                    SFO_lag1=SFO_lag1[T],SFO_lag2=SFO_lag2[T], INR_lag1=INR_lag1[T],INR_lag2=INR_lag2[T], USDX_lag1=USDX_lag1[T],USDX_lag2=USDX_lag2[T])
view(newdata1)
## Use the predict() command to forecast out the next observation in period T+1 out of the sample
CPO_fcast_out=predict(reg6, newdata=newdata1) 

## Standard error of search_fcast_in_error=RMSFE (see slides 27 and 28 in Lecture Note 9)
RMSFE=sqrt(var(residuals(reg7)))

## 95% CI for out-of-sample forecast
CPO_fcast_out_CIL=CPO_fcast_out-1.96*RMSFE
CPO_fcast_out_CIH=CPO_fcast_out+1.96*RMSFE

## Output results
sprintf("Out-of-sample forecast for period T=131: %f", CPO_fcast_out)
sprintf("95 CI lower bound for out-of-sample forecast: %f", CPO_fcast_out_CIL)
sprintf("95 CI upper bound for out-of-sample forecast: %f", CPO_fcast_out_CIH)

CPO_fcast_out   
