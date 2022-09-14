library(fpp2)
library(tseries)
library(vars)

# this needs to be set to correct directory
setwd("~/Downloads")

# loading data from excel
term_project <- readxl::read_excel("Texas County Sales Taxes.xlsx", sheet = "Grayson")

# creating training time series
training <- ts(term_project[, "Sales_Tax"], frequency=12, start=c(2011, 11), end=c(2021, 7))

# full time series
sales_tax <- ts(term_project[, "Sales_Tax"], frequency=12, start=c(2011, 11))

# basic time series plot
options(scipen = 999)
autoplot(sales_tax)+
  ggtitle("Grayson County Sales Tax")+
  xlab("Year")+
  ylab("Sales Tax")

# creating ETS model
model1 <- ets(training, model="ZZZ")

# creating ARIMA model
model3 <- auto.arima(training)

#Residual Diagnostics
accuracy(model1)
accuracy(model2)

# creating forecast variables for both
for1 <- forecast(model1, h=6)
for3 <- forecast(model3, h=6)

#obtaining forecast point values
for1
for3

# overlaying ETS model on training plot
autoplot(sales_tax)+
    autolayer(for1)+
  ggtitle("ETS Forecast 8/21 - 1/22")+
  xlab("Year")+
  ylab("Sales Tax")

# overlaying ARIMA model on training plot
autoplot(sales_tax)+
    autolayer(for3)+
  ggtitle("ARIMA Forecast 8/21 - 1/22")+
  xlab("Year")+
  ylab("Sales Tax")

# comparing accuracy of ETS and ARIMA
accuracy(for1)
accuracy(for3)

#ETS one step plots 
pf.ETS.term_project <- readxl::read_excel("Texas County Sales Taxes.xlsx", sheet = "PF - ETS")
pf.ETS.full <- ts(pf.ETS.term_project[, "Sales_Tax"], frequency=12, start=c(2011, 11))
pf.ETS.training <- ts(pf.ETS.term_project[, "Sales_Tax"], frequency=12, start=c(2011, 11), end=c(2021, 7))
pf.ETS.forecast <- window(pf.ETS.full, start=c(2021, 8), end=c(2022, 1))

autoplot(pf.ETS.training)+
  autolayer(pf.ETS.forecast)+
  ggtitle("ETS One-Step Ahead 8/21-1/22")+
  ylab("Sales_Tax")+
  xlab("Year")

#ARIMA one step plots 
pf.ARIMA.term_project <- readxl::read_excel("Texas County Sales Taxes.xlsx", sheet = "PF - ARIMA")
pf.ARIMA.full <- ts(pf.ARIMA.term_project[, "Sales_Tax"], frequency=12, start=c(2011, 11))
pf.ARIMA.training <- ts(pf.ARIMA.term_project[, "Sales_Tax"], frequency=12, start=c(2011, 11), end=c(2021, 7))
pf.ARIMA.forecast <- window(pf.ARIMA.full, start=c(2021, 8), end=c(2022, 1))

autoplot(pf.ARIMA.training)+
  autolayer(pf.ARIMA.forecast)+
  ggtitle("ARIMA One-Step Ahead 8/21-1/22")+
  ylab("Sales_Tax")+
  xlab("Year")

#12 Month Ahead Models
model5 <- ets(sales_tax, model="ZZZ")
model6 <- auto.arima(sales_tax)

#12 Month Ahead Forecasts
for5 <- forecast(model5, h=12)
for6 <- forecast(model6, h=12)

#Individual Values for Each Forecast
for5
for6

#Plotting ETS 12 Month Ahead
autoplot(sales_tax)+
  autolayer(for5)+
  ggtitle("ETS 12 Month Ahead Forecast")+
  xlab("Year")+
  ylab("Sales Tax")

#Plotting ARIMA 12 Month Ahead
autoplot(sales_tax)+
  autolayer(for6)+
  ggtitle("ARIMA 12 Month Ahead Forecast")+
  xlab("Year")+
  ylab("Sales Tax")

###VAR MODEL###

#Importing 2 Additional Time Series
unemrate <- ts(term_project[, "Unemployment Rate"], frequency=12, start=c(2011, 11))
clf <- ts(term_project[, "Civilian Labor Force"], frequency=12, start=c(2011, 11))

#Checking for Differences
ndiffs(unemrate)
ndiffs(clf)
ndiffs(sales_tax)

#Difference Each Series
d_unemrate <- diff(unemrate)
d_clf <- diff(clf)
d_sales_tax <- diff(sales_tax)

#Creating Matrix for VAR
var.st <- matrix(c(d_unemrate, d_clf, d_sales_tax), nrow = length(d_sales_tax), dimnames = list(c(1:122), c("Unemployment Rate", "Civilian Labor Force", "Sales Tax")))
ts.matrix <- as.ts(var.st)

#VAR Model 
VARselect(ts.matrix, lag.max = 12)
model9 <- VAR(ts.matrix, p=2, type = "const")
summary(model9)
for9 <- forecast(model9, h=12)
autoplot(for9)


