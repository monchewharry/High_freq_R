# (E(R^p))^(1/p) = C*deltaT^(D)  

load("../eurusd20151.RData")
suppressMessages(library(quantmod))
library(dplyr)
#take cl as the previous tick interpolation
fxx <- as.data.frame(fx)
fxx$Tickreturn <- c(0,diff(fxx$Mid))#mid price log return
fx <- .xts(fxx,index = index(fx))  

####1 min price.ohlc######
#align time

getsd<-function(k,p){
  fx.1 <- align.time(fx$Tickreturn[-1],k)## Create 1-minute bars 
  ret<-aggregate(fx.1,by = index(fx.1),FUN = sum)
  (mean(abs(ret)^(p)))^(1/p)
}

vol<-c()
for(i in seq(60)){
  v<-getsd(k = i,p = 1)
  print(i)
  vol<-c(vol,v)
}

(m <- lm(log(vol)~log(seq(60))))

png("basic_Stat/scale.png",width = 1000)
plot(log(seq(60)),log(vol))
abline(m$coefficients)
dev.off()
