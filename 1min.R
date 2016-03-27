load("eurusd20151.RData")
suppressMessages(library(quantmod))
#take cl as the previous tick interpolation

####1 min price.ohlc######
#align time
k=1
fx.1 <- align.time(fx,60*k)## Create 1-minute bars 
#create ohlc
fx.ohlc <- to.minutes(fx.1[,"Mid"],k = k)# to an OHLC series
colnames(fx.ohlc) <- c("Open","High","Low","Close")

fx.ohlc <- merge(fx.ohlc[endpoints(fx.ohlc,'minutes')]
                     ,xts( ,seq(start(fx.ohlc)
                                ,end(fx.ohlc),by="1 mins")))## Fill-in gaps in 1-minute bars

fx.ohlc[,"Close"] <- na.locf(fx.ohlc[,"Close"]) ## Fill-in NAs in gaps  
fx.ohlc[is.na(fx.ohlc[,"Open"]),"Open"] <-
 fx.ohlc[is.na(fx.ohlc[,"Open"]),"Close"]
fx.ohlc[is.na(fx.ohlc[,"Low"]),"Low"] <-
 fx.ohlc[is.na(fx.ohlc[,"Low"]),"Close"]
fx.ohlc[is.na(fx.ohlc[,"High"]),"High"] <-
 fx.ohlc[is.na(fx.ohlc[,"High"]),"Close"]

## quantmod::chart
clfx<-Cl(fx.ohlc)['2015-01-05 05:00::2015-01-07 13:00',]
chartSeries(clfx
            ,theme=chartTheme("white",bg.col=0)
            ,name="eurusd20151")

####1 min Returns tail#####  
rm(list = ls(envir = .GlobalEnv)[-grep(pattern = "^fx$",x = ls(envir = .GlobalEnv))])
fxx <- as.data.frame(fx)
fxx$Tickreturn <- c(0,diff(fxx$Mid))#mid price log return
fx <- .xts(fxx,index = index(fx))

k=1
Ret <- align.time(fx$Tickreturn ,60*k)# CREATE ONE minute bars
minRet <-sapply(split(Ret, index(Ret)), sum)#1min return
t <- as.POSIXct(strptime(names(minRet),"%Y-%m-%d %H:%M:%S"))
minRet <-.xts(minRet,index = t)

chartSeries(minRet
            ,theme=chartTheme("white",bg.col=0)
            ,name="tick Return")
(a<-acf(minRet,lag.max = 20))#negative autocorrelation 
plot(density(minRet))

library(moments)
skewness(minRet)#should be close to 0
kurtosis(minRet)#should be close to 0
summary(minRet)
plot(density(minRet))

####return seasonality ####  
#autocorrelation of hourly return
k=60
Ret <- align.time(fx$Tickreturn ,60*k)# CREATE ONE HOUR bars
HourRet <-sapply(split(Ret, index(Ret)), sum)
t <- as.POSIXct(strptime(names(HourRet),"%Y-%m-%d %H:%M:%S"))
HourRet <-.xts(HourRet,index = t)
acf(abs(HourRet))
acf(HourRet^2)

####return volatility ##### 
k=1
Ret <- align.time(fx$Tickreturn ,60*k)# CREATE ONE minute bars
minRet <-sapply(split(Ret, index(Ret)), sum)#1min return
t <- as.POSIXct(strptime(names(minRet),"%Y-%m-%d %H:%M:%S"))
minRet <-.xts(minRet,index = t)

hourRet <- align.time(minRet,60*60)# CREATE ONE HOUR bars
v <- function(x,p){
  (mean(abs(x)^p))^(1/p)
}
 
oneday<- hourRet
volatility <-sapply(split(oneday, index(oneday)), v, p=1)
t <- as.POSIXct(strptime(names(volatility),"%Y-%m-%d %H:%M:%S"))
volatility <-.xts(volatility,index = t)
barplot(volatility)#u shape volatility




