require(data.table)
require(TSA)
require(forecast)
require(xts)
require(tseries)
require(graphics)

train <- fread("../input/train_1.csv")

# To analyze and build the model, Lets start by picking the page with maximum number of visit.
train$sum = rowSums(train[,2:551])
max(na.omit(train$sum))
which(train$sum == 12066181102)
trainsep = train[38574,]

f = t(trainsep[,-c(1,552)])

f = data.frame(f,row.names(f))
colnames(f) = c("f","d")

f$d = as.POSIXct(f$d, format="%Y-%m-%d")
t2_xts = xts(f$f, order.by = f$d)
start_date = as.POSIXct("2015-07-01", format="%Y-%m-%d")
end_date = as.POSIXct("2016-11-30", format="%Y-%m-%d")

**#Training set till 2016-11-30**
t2_tr = window(t2_xts, start = start_date, end = end_date)

#test set from 2016-12-01 till 2016-12-31 
start_date = as.POSIXct("2016-12-01", format="%Y-%m-%d")
end_date = as.POSIXct("2016-12-31", format="%Y-%m-%d")

t2_te = window(t2_xts, start = start_date, end = end_date)
#Converting the xts class to data frame format
t2_tr = data.frame(index(t2_tr),t2_tr)
#Changing the column names of t2_tr
colnames(t2_tr) = c("d","f")
rownames(t2_tr) = rownames("")

t2_te = data.frame(index(t2_te),t2_te)
#Changing the column names of t2_te
colnames(t2_te) = c("d","f")
rownames(t2_te) = rownames("")

#before proceeding lets check on stationarity 
#library(tseries)
adf.test(t2_tr$f)

#Augmented Dickey-Fuller Test

#data:  t2_tr$f
#Dickey-Fuller = -3.7176, Lag order = 8, p-value = 0.02312
#alternative hypothesis: stationary

# test shows that our data is stationarity.

#The data is from 2015-07-01 to 2016-12-31. there for it is less than two years and totally of 15 months of data. 
# we can apply weekly seasonality and monthly seasonality.  if the data is more than two years, we can also apply yealy seasonality.


# Theoretically there are only two seasonality 
y_2 <- msts(t2_tr$f, seasonal.periods=c(7,4.34*7))

plot(stl(y_2,s.window="periodic",robust=TRUE))
#plot clearly showed the seasonal and trend part.

#let’s compare the four different models. 

#TBATS
tbats.model = tbats(y_2)
tbats.forecast = forecast(tbats.model,h=31)
plot(tbats.forecast)

#Lets check on accuracy part 
accuracy(tbats.forecast$mean,t2_te$f)

#              ME    RMSE     MAE      MPE     MAPE
#Test set 2997230 3423985 3045547 12.13825 12.39839

#in real life absolute error rate may add more value to business.
tbatforecast = data.frame(t2_te$d,t2_te$f,tbats.forecast$mean)
colnames(tbatforecast) = c("d","actuals","tbats.forecast")

tbats.abs.error = abs(sum(tbatforecast$actuals)-sum(tbatforecast$tbats.forecast))/sum(tbatforecast$actuals)
tbats.abs.error

#0.1271615
#TBATS have performed approximatly 12% of error. 

#Lets have a look on the error 
#require(graphics)

plot.zoo(cbind(tbatforecast$actuals, tbatforecast$tbats.forecast), 
         plot.type = "single", 
         col = c("red", "blue"))

## From the graph we can see that the model was able to capture the seasonality but not the peaks. 
## Lets explore and confirm on residuals

tbatforecast$Residual = abs(tbatforecast$actuals - tbatforecast$tbats.forecast)
plot.ts(tbatforecast$Residual)
## by residual plot we confirm that the model is not able to predict the peak values.
## Lets have the TBATS as the bench mark.

#BATS Model
bats.model = bats(y_2)
bats.forecast = forecast(bats.model,h=31)
plot(bats.forecast)

#Lets check on accuracy part 
accuracy(bats.forecast$mean,t2_te$f)

#              ME    RMSE     MAE     MPE     MAPE
#Test set 3738600 4147897 3748747 15.2777 15.33233

#in real life absolute error rate may add more value to business.
forecastValue = data.frame(t2_te$d,t2_te$f,tbats.forecast$mean,bats.forecast$mean)
colnames(forecastValue) = c("d","actuals","tbats.forecast","bats.forecast")

bats.abs.error = abs(sum(forecastValue$actuals)-sum(forecastValue$bats.forecast))/sum(forecastValue$actuals)
bats.abs.error

#0.1586151
#BATS have perform badly when compared to TBATS.

#Lets have a look on the error 

#require(graphics)
plot.zoo(cbind(forecastValue$actuals, forecastValue$bats.forecast), 
         plot.type = "single", 
         col = c("red", "blue"))

## from the graph we can see that the model does not able to capture anything. 
## Lets explore and confirm on residuals

batsResidual = abs(forecastValue$actuals - forecastValue$bats.forecast)
plot.ts(batsResidual)

#STLM with ARIMA 
stlm.model = stlm(y_2,s.window="periodic")
stlm.forecast = forecast(stlm.model,h=31)
plot(stlm.forecast)


#Lets check on accuracy part 
accuracy(stlm.forecast$mean,t2_te$f)

#              ME    RMSE     MAE     MPE     MAPE
#Test set 4115535 4577968 4122950 16.7827 16.82066

#The model is really performing poor.
#in real life absolute error rate may add more value to business.
forecastValue = data.frame(t2_te$d,t2_te$f,tbats.forecast$mean,stlm.forecast$mean,bats.forecast$mean)
colnames(forecastValue) = c("d","actuals","tbats.forecast","stlm.forecast","bats.forecast")

stlm.abs.error = abs(sum(forecastValue$actuals)-sum(forecastValue$stlm.forecast))/sum(forecastValue$actuals)
stlm.abs.error
#0.1746071
#STLM have perform badly when compared to Tbats. 

#Lets have a look on the error 
plot.zoo(cbind(forecastValue$actuals, forecastValue$stlm.forecast), 
         plot.type = "single", 
         col = c("red", "blue"))

## from the graph we can see that the model does not able to capture anything. 
## Lets explore and confirm on residuals

stlmResidual = abs(forecastValue$actuals - forecastValue$stlm.forecast)
plot.ts(stlmResidual)


#ARIMA

bestfit = list()
bestfit <- list(aicc=Inf)

for(i in 1:3) {
for (j in 1:3){
f1xreg <- fourier(ts(t2_tr$f, frequency=7), K=i)
f2xreg <- fourier(ts(t2_tr$f, frequency=7*4.34), K=j)
arima.model <- auto.arima(t2_tr$f, xreg=cbind(f1xreg, f2xreg), seasonal=F)
if(arima.model$aicc < bestfit$aicc) {
bestfit <- list(aicc=arima.model$aicc, i=i, j=j, fit=arima.model)
}
}
}

xregm=cbind(fourier(ts(t2_tr$f, frequency=7), K=bestfit$i, h=31),
fourier(ts(t2_tr$f, frequency=7*4.34), K=bestfit$j, h=31))

arima.forecast <- forecast(bestfit$fit, xreg=xregm)

plot(arima.forecast)


#Lets check on accuracy part 
accuracy(arima.forecast$mean,t2_te$f)

#              ME    RMSE     MAE     MPE     MAPE
#Test set 4439896 4804145 4439896 18.27674 18.27674

#The model is really performing poor.
#in real life absolute error rate may add more value to business.
forecastValue = data.frame(t2_te$d,t2_te$f,tbats.forecast$mean,stlm.forecast$mean,bats.forecast$mean,arima.forecast$mean)
colnames(forecastValue) = c("d","actuals","tbats.forecast","stlm.forecast","bats.forecast","arima.forecast")

arima.abs.error = abs(sum(forecastValue$actuals)-sum(forecastValue$arima.forecast))/sum(forecastValue$actuals)
arima.abs.error

#0.1883685
#ARIMA have perform badly whem compared to Tbats. 

#Lets have a look on the error 

#require(graphics)
plot.zoo(cbind(forecastValue$actuals, forecastValue$arima.forecast), 
         plot.type = "single", 
         col = c("red", "blue"))

## from the graph we can see that the model does not able to capture anything. 
## Lets explore and confirm on residuals

arimaResidual = abs(forecastValue$actuals - forecastValue$arima.forecast)
plot.ts(arimaResidual)


plot.zoo(cbind(ts(forecastValue$actuals),ts(forecastValue$tbats.forecast),
               ts(forecastValue$bats.forecast),ts(forecastValue$stlm.forecast),ts(forecastValue$arima.forecast)), 
         plot.type = "single", 
         col = c("red", "blue","green","black","pink"))


### from the graph we can conclude that TBATS have perform better than the other model.
