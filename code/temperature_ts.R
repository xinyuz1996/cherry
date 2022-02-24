rm(list=ls())

# time series forecasting
# library(rworldmap) #???? may not be used
library(TSA) # eacf
library("tseries") # adf.test
library(forecast) #auto.arima
library(dplyr)
library(fpp3) # time series models
library(feasts)

cherry <- read.csv("../data/FinalizedData/FinalData/DC.csv")
# cherry <- read.csv("../data/washingtondc.csv") %>% 
#   bind_rows(read.csv("../data/liestal.csv")) %>% 
#   bind_rows(read.csv("../data/kyoto.csv"))
str(cherry)
precip = cherry$PRCP
temp = cherry$TAVG
date = (as.Date(as.character(cherry$DATE)))
year = cherry$year
data = data.frame(precip, temp, date, year)
str(data)

?ts()
length(date)
head(data,2)
tail(data,2)
data_year = data %>% filter(year==2000)

#### Step 1: Preview
# Use ACF to identify the type of time series
acf(ts(data_year$temp), lag.max = 100, col = 'dark blue', bg = 'dark blue', 
    panel.first = grid(), main = 'ACF Plot of Adj. Price of AAPL', lwd=.01)
# Based on acf plot, we know that it is more likely to be an AR process
pacf(ts(data_year$temp), lag.max = 10, col = 'dark blue', bg = 'dark blue', 
     panel.first = grid(), main = 'PACF Plot of Adj. Price of AAPL')
# Since PACF damp out for AR(p) process when lag > p, we could guess that 
# it follows AR(p) process with p around 1 (or maybe 3? Or maybe ARMA)
adf.test(ts(data_year$temp))
# This means the trend has to be estimated

plot(x=data$date, y=ts(data$temp), main = "TS Plot of Temperature", type="l",
     col = 'dark blue', bg = 'dark blue',  panel.first = grid(), lty=1, ylab="Temperature", xlab="Day")

plot(x=data_year$date, y=ts(data_year$temp), main = "TS Plot of Temperature in 2020", type="l",
     col = 'dark blue', bg = 'dark blue',  panel.first = grid(), lty=1, ylab="Temperature", xlab="Day")

data_ts = as_tsibble(data, index=date)
data_ts %>%
  autoplot(temp, col="dark blue") +
  labs(x="Date", y = "Temperature", title = "Time series")

#'  Trend decomposition using additive model since the The additive 
#'  decomposition is the most appropriate if the magnitude of the 
#'  seasonal fluctuations, or the variation around the trend-cycle,
#'   does not vary with the level of the time series.

#### Step 2 STL Decomposition
dcmp <- data_ts %>% 
  model(stl = STL(temp ~ trend(window=7), robust=TRUE)) %>%
  components() %>%
  select(-.model)
dcmp

#### Step 3 Forecasting
h = 30
fcst = dcmp %>%
  model(SNAIVE(temp~lag("year"))) %>%
  forecast(h=h)

fcst %>% autoplot(dcmp) +
  labs(y = "Temperature",
       title = "TS Forecast of Temperature")

temprature_fcst = fcst$.mean


date.f = seq(from=tail(data$date,1), to=tail(data$date,1)+h, length.out=h)
data.forecast = data.frame(temp=temprature_fcst, date=date.f)
data_total = rbind(data[,c('temp','date')],data.forecast)
str(data_total)

write.csv(data_total,"../data/temp_forecast.csv")





#### time series

beta0 = c(-280.08748249, -250.17648572, -310.13513578, -176.82744787,
       -152.95793016, -167.04313605, -326.51569084, -175.52126718,
       -106.76351411, -234.71521583, -169.4254379 , -198.79201096,
       -259.92757024, -465.71192393, -261.31640609, -353.64453504,
       -333.92239345, -231.05418157, -199.98655687, -230.93356069,
       -262.02752348, -163.02338885, -222.47279548, -161.09562497,
       -233.60293469, -153.58760558, -113.63787838, -272.85464508,
       -228.77625924, -239.83357604, -197.43552975, -121.38247144,
       -173.53608265, -192.47507819, -217.16998023, -231.93332415,
       -431.75072467, -278.25212287, -308.91322631, -287.4463173 ,
       -168.92908464, -169.39543163, -188.19669916, -239.89359091,
       -207.06346815, -267.23371307, -293.30355958, -198.60158523,
       -228.91742371, -167.07238986, -159.21349872, -144.49851427,
       -151.1508533 , -208.6450107 , -291.56565046, -252.91688135,
       -244.94548695, -146.73587405, -288.09404693, -154.60313102,
       -259.34866855, -214.30064519, -287.26420128, -288.00558743)


b0test = c(-213.35624504, -305.8333546 , -121.85833012, -221.34027378,
         -253.1834163 , -276.76911814, -207.27659952, -241.09567642,
         -185.37155536, -190.23513033, -286.5742323 , -244.78130958,
         -200.67928357, -178.37837756, -214.21793183, -182.34473253,
         -247.76783744, -320.22237049, -353.92454901, -232.49670802,
         -207.75167897)
err = numeric()
par(mfrow=c(2,3))
for(o in 1:20){
  b0m1 = ar(beta0, aic=FALSE,order.max=o)
  p = predict(b0m1, n.ahead=length(b0test))
  model.pred = p$pred
  err[o] = mean((model.pred-b0test)^2)
  # plot(model.pred, main=paste("AR(",o,")", sep=""))
}
cbind(order(err, decreasing = FALSE), sort(err, decreasing = FALSE))
library(forecast)
b0m2 = auto.arima(beta0)

