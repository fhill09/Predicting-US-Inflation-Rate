#' ---
#' author: Bloti B. Teh, Faith Hill and Huy Nguyen
#' ---

# OR 688 Fall 2022
# Final Project: Consumer Price Index & 
# Due Date: 12/5/2022 Monday 11:59 PM


#par(mar = c(1, 1, 1, 1))        # Changing area margins

# Loading and/or installing needed packages
if( !require('tidyverse') ){
  install.packages('tidyverse', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(tidyverse)

if( !require('tibble') ){
  install.packages('tibble', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(tibble)

if( !require('RColorBrewer') ){
  install.packages('RColorBrewer', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(RColorBrewer)


if( !require('GGally') ){
  install.packages('GGally', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(GGally)

if( !require('gridExtra') ){
  install.packages('gridExtra', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(gridExtra)

if( !require('ggplot2') ){
  install.packages('ggplot2', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(ggplot2)

if( !require('dplyr') ){
  install.packages('dplyr', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(dplyr)

if( !require('forecast') ){
  install.packages('forecast', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(forecast)

if( !require('Ecdat') ){
  install.packages('Ecdat', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(Ecdat)

if( !require('sandwich') ){
  install.packages('sandwich', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(sandwich)

if( !require('fracdiff') ){
  install.packages('fracdiff', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(fracdiff)


set.seed(0) 

#

# Create data frames from csv files

df <- read.csv(
  file="CCPIU.csv",
  header=TRUE, as.is=TRUE)

## # Exploratory Data Analysis (EDA)
# List variables on each year-specific data frame
names(df)

# Display data summary and statistics
str(df)
summary(df)
dim(df)
head(df, 10)
nrow(df)

# Examining data quality 

describe_vars <- function(df) {
# Describes the variables in data frame df

# Number of missing and distinct values
missing <- df %>%
  summarize(across(everything(),
                   ~sum(is.na(.))))
distinct <- df %>%
  summarize(across(everything(),
                   n_distinct))
# Whether numeric for each variable 
nc <- ncol(df) 
numeric <- vector("logical", nc)
for (i in 1:nc) {
  numeric[i] <- is.numeric(df[1,])
}

# Create output data frame 
df2 <- data.frame("colNum"=(1:nc),
"var" = names(df), "nonmissing" =
  nrow(df)-t(missing),
"missing" = t(missing),
"distinct" = t(distinct), numeric)
return(df2) }

describe_vars(df)


# Seasonal ARIMA Model:
# 

ccpiu_value = df[,2] 

# Consider consumption on its own:
# 
par(mfrow=c(1,4))
acf( ccpiu_value ) 
acf( diff(ccpiu_value,1) )
acf( diff(ccpiu_value,4) ) 
acf( diff(diff(ccpiu_value,1),4) ) 
par(mfrow=c(1,1))

# Consider the logarithm of consumption:
#
lccpiu_value = log(ccpiu_value)

par(mfrow=c(1,4))
acf( lccpiu_value ) 
acf( diff(lccpiu_value,1) )
acf( diff(lccpiu_value,4) ) 
acf( diff(diff(lccpiu_value,1),4) ) 
par(mfrow=c(1,1))

# Fit the model that looks most likely:
# 
ccpiu_value_ts_model = arima( ccpiu_value, order=c(0,1,0), 
                              seasonal=list(order=c(0,1,1), period=4) )
Box.test( residuals(ccpiu_value_ts_model), lag=10, type="Ljung" ) 

log_ccpiu_value_ts_model = arima( lccpiu_value, order=c(0,1,0), 
                                  seasonal=list(order=c(0,1,1), period=4) )
Box.test( residuals(log_ccpiu_value_ts_model), lag=10, type="Ljung" ) 

# ACF of the residuals 
# Residual autocorrelation

acf( residuals(log_ccpiu_value_ts_model) )

# Application of auto.arima() to log(consumption) using BIC. Which
# Determination of best model ( model selected
# 
auto.arima( lccpiu_value, ic="bic" )

# # Prediction of arima object (an object returned by the arima()
# function. Use of the the predict function. 
# Forecast an object returned by auto.arima(), 
# We use the forecast() function in the forecast package.

# 
ccpiu_value_prediction_m_1 = predict( ccpiu_value_ts_model, n.ahead=8 ) 
ccpiu_value_prediction_m_2 = predict( log_ccpiu_value_ts_model, n.ahead=8 )

m_1_pred = ccpiu_value_prediction_m_1$pred
m_1_ll = m_1_pred - 2*ccpiu_value_prediction_m_1$se
m_1_ul = m_1_pred + 2*ccpiu_value_prediction_m_1$se

m_2_pred = exp(ccpiu_value_prediction_m_2$pred )
m_2_ll = exp( ccpiu_value_prediction_m_2$pred - 2*ccpiu_value_prediction_m_2$se )
m_2_ul = exp( ccpiu_value_prediction_m_2$pred + 2*ccpiu_value_prediction_m_2$se )

plot_y_max = max( c( m_1_ul, m_2_ul ) )
plot_y_min = min( c( m_1_ll, m_2_ll ) )

plot( m_1_pred, col='red', ylim=c(plot_y_min,plot_y_max) )
points( m_1_ll, type='l', lty=2, col='red' )
points( m_1_ul, type='l', lty=2, col='red' ) 

points( m_2_pred, type='l', col='green' )
points( m_2_ll, type='l', lty=2, col='green' )
points( m_2_ul, type='l', lty=2, col='green' ) 

grid()

# Long-Memory Processes
#Here, this section uses changes in the square root of the Consumer Price Index.
# Code below code creates th time series.

cpi = as.vector(df[,2])
DiffSqrtCpi = diff(sqrt(cpi))

# Plot DiffSqrtCpi and its ACF
par(mfrow=c(1,2))
plot( DiffSqrtCpi, type='l', col='blue' )
acf( DiffSqrtCpi )
par(mfrow=c(1,1))

# Estimation of the amount of fractional differencing,
# fractionally difference DiffSqrtCpi appropriately, and check the ACF of the
# fractionally differences series.

fit.frac = fracdiff(DiffSqrtCpi,nar=0,nma=0)
fit.frac$d
fdiff = diffseries(DiffSqrtCpi,fit.frac$d)
acf(fdiff)
