cpi=read.csv('processed_CPI.csv')
gold=read.csv('processed_gold.csv')
treasury=read.csv('processed_treasury.csv')
inflation=read.csv('processed_Inflation.csv')
oil=read.csv('processed_WTI_price.csv')
#load SP500
library(tidyquant)
library(quantmod)
SP500 = tq_get("^GSPC", from = '1990-01-01')

#merge data together
library(tidyverse)

#cast date column to be date
cpi$date <- as.Date(cpi$date,format = '%m/%d/%Y')
inflation$date <- as.Date(inflation$date,format = '%Y-%m-%d')
gold$date <- as.Date(gold$date, format = '%m/%d/%Y')
treasury$date <- as.Date(treasury$date, format = '%m/%d/%Y')
oil$date <- as.Date(oil$date, format = '%m/%d/%Y')

#adding year and month columns in the dataset
library(lubridate)

cpi$year <- year(cpi$date)
cpi$month <- month(cpi$date)

gold$year <- year(gold$date)
gold$month <- month(gold$date)

SP500$year <- year(SP500$date)
SP500$month <- month(SP500$date)

oil$year <- year(oil$date)
oil$month <- month(oil$date)

treasury$year <- year(treasury$date)
treasury$month <- month(treasury$date)


# Aggregate function in R with mean summary statistics
treasury_m <- aggregate(value ~ year + month, data = treasury, FUN = mean, na.rm=TRUE)
SP500_m <- aggregate(adjusted ~ year + month, data = SP500, FUN = mean, na.rm=TRUE)  
oil_m <- aggregate(Oil_price ~ year + month, data = oil, FUN = mean, na.rm=TRUE)  
gold_m <- aggregate(daily_avg ~ year + month, data = gold, FUN = mean, na.rm=TRUE)  


#merge all data frames together
alldata <- cpi %>%
  full_join(inflation, by='date') %>%
  full_join(gold_m, by=c('year','month')) %>%
  full_join(treasury_m, by=c('year','month')) %>%
  full_join(SP500_m, by=c('year','month'))%>%
  full_join(oil_m, by=c('year','month'))%>%
  drop_na()

#dropping not needed columns
df = subset(alldata, select = -c(Year,Month) )

#rename column name
df = rename(df,'CPI.value'= 'Value','gold.avg'='daily_avg','bond.rate'='value')

#organized columns
df <- df[, c(1,3,4,2,5,6,7,8,9)]

#plotting correlation between the variables
library(corrplot)
corrplot(cor(df[,6:10]))
pairs(df[,6:10])

# Calculate the diff in the data-- S&P returns
Inflation_diff=diff(df$Inflation_rate)
CPI_diff= diff(df$CPI.value)
gold_diff = diff(log(df$gold.avg))
bond_diff = diff(df$bond.rate)
oil_diff = diff(df$Oil_price)
SP500_return = diff(log(df$adjusted))


#testing normality
jarque.bera.test(CPI_diff)
jarque.bera.test(gold_diff)
jarque.bera.test(bond_diff)
jarque.bera.test(oil_diff)
jarque.bera.test(SP500_return)



#plotting graph each variables
par(mfrow=c(1,1))

plot(Inflation_diff, main="Change in Inflation")
plot(SP500_return, main="GSPC (S&P500) return")
plot(CPI_diff, main="Change in CPI")
plot(gold_diff, main="Gold return")
plot(oil_diff, main="Change in oil price")
plot(bond_diff, main="Change in bond price")

#plotting the change in value
df_diff<-cbind(Inflation_diff,SP500_return,CPI_diff,gold_diff,bond_diff,oil_diff)
corrplot(cor(df_diff))
plot(as.data.frame(df_diff))

#apply the regression model 
fit1 = lm(Inflation_diff ~ CPI_diff)
fit2 = lm(Inflation_diff ~ CPI_diff + SP500_return)
fit3 = lm(Inflation_diff ~ CPI_diff + SP500_return + gold_diff)
fit4 = lm(Inflation_diff ~ CPI_diff + SP500_return + gold_diff + bond_diff)
fit5 = lm(Inflation_diff ~ CPI_diff + SP500_return + gold_diff + bond_diff + oil_diff)

anova(fit5)

#calculate best fit 
#One-Variable Model
AIC(lm(Inflation_diff ~ CPI_diff)) #-54331
AIC(lm(Inflation_diff ~ SP500_return))
AIC(lm(Inflation_diff ~ gold_diff))
AIC(lm(Inflation_diff ~ bond_diff))
AIC(lm(Inflation_diff ~ oil_diff))

# Two-Variable Model
AIC(lm(Inflation_diff ~ CPI_diff + SP500_return))#54485
AIC(lm(Inflation_diff ~ gold_diff + SP500_return))
AIC(lm(Inflation_diff ~ gold_diff + CPI_diff))
AIC(lm(Inflation_diff ~ oil_diff + CPI_diff))
AIC(lm(Inflation_diff ~ oil_diff + gold_diff))
AIC(lm(Inflation_diff ~ oil_diff + SP500_return))

# Three-Variable Model
AIC(lm(Inflation_diff ~ CPI_diff + oil_diff + SP500_return)) #-54742
AIC(lm(Inflation_diff ~ gold_diff + oil_diff + SP500_return))
AIC(lm(Inflation_diff ~ gold_diff + CPI_diff + SP500_return))

# Four-Variable Model
AIC(lm(Inflation_diff ~ CPI_diff + oil_diff + SP500_return + gold_diff)) #-54740
AIC(lm(Inflation_diff ~ gold_diff + oil_diff + SP500_return + bond_diff))
AIC(lm(Inflation_diff ~ gold_diff + CPI_diff + SP500_return + bond_diff))

# Five-Variable Model
AIC(lm(Inflation_diff ~ gold_diff + CPI_diff + SP500_return + bond_diff + oil_diff)) #-54792

summary(fit5)


#using PCA to do analysis
pca = prcomp(df_diff)
summary(pca)
plot(pca,main="PCA")
fit6 = lm(Inflation_diff ~ pca$x[,1] + pca$x[,2])

AIC(fit6) #-54687

pca

# loading catools package for splitting data
library(caTools)
#converting to dataframe combining
df_diff_com=as.data.frame(df_diff)

#standadize data to have mean = 0 and standard deviation = 1
df_diff1 <- df_diff_com %>% mutate_all(~(scale(.) %>% as.vector))
df_diff1

ind = sample.split(Y = df_diff1$gold_diff, SplitRatio = 0.7)

#subsetting into Train data
train = df_diff1[ind,]


#subsetting into Test data
test = df_diff1[!ind,]

#applying linear regression to train data
linear_model <- lm(Inflation_diff ~ gold_diff + CPI_diff + SP500_return + bond_diff + oil_diff, data = train)

#making prediction for the change in inflation rate
predict_df<-predict(linear_model, newdata = test, interval = 'confidence')

#copy actual result into the preidct_df
predict_df<-as.data.frame(predict_df)
predict_df$actual_res <- test$Inflation_diff

#checking if the value prediction is in range

predict_df<-predict_df%>%  mutate(ok = predict_df$actual_res  >= predict_df$lwr & predict_df$actual_res <=  predict_df$upr)

predict_df$ok<- as.integer(as.logical(predict_df$ok))

#percentage of accuracy
sum(predict_df$ok)/length(predict_df$ok)
