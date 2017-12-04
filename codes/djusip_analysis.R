setwd('D:\\Project\\OR538 Project\\dataset')

#===============================#
#      DJUSIP Analysis          #
#===============================#

# data loading

final = read.csv('final_dataset.csv', header = T)
SplitDates = read.csv('SplitDate.csv', header = T)
DJI = final$DJI
DJUSIP = final$DJUSIP
volume = final$DJUSIP_Volume

# data transfer
final$During = as.character(final$During)
final$Strong = as.character(final$Strong)
final$Death = as.character(final$Death)
final$high_temp = as.character(final$high_temp)
final$season = as.character(final$season)

SplitDates$when = as.Date(SplitDates$when)

dat = final[,-c(1,11,12,13,14,15)]

# data checking
str(dat)

# install.packages('corrplot',dependencies = T)
library(corrplot)
cor(as.matrix(final[,c(2,3,4,9)]))
dat = dat[,-c(2,3)]

str(dat)

#######
#Return
#######

# abnormal return
DJIreturn = diff(log(final$DJI))
DJUSIPreturn = diff(log(final$DJUSIP))

m = lm(DJUSIPreturn~DJIreturn)
abreturn = DJUSIPreturn - m$coefficients[1]-m$coefficients[2]*DJIreturn

plot(ts(abreturn,start=2001,frequency = 252),xlim=c(2001,2018),axes=F)
axis(1,at=2001:2018,labels=2001:2018);axis(2);box()

n = dim(data.frame(abreturn))[1]

# abreturn linear model
dat2 = cbind(dat[-n,],abreturn)
m2 = lm(abreturn~.-abreturn,data=dat2)
summary(m2)
# which indicates that I use abnormal return
# is a very bad choice to invest the correlations

# abreturn vs AR(1)
abreturn.t=abreturn[-n]
abreturn.t_1=abreturn[-1]
dat3 = cbind(dat[-c(n,n-1),],abreturn.t,abreturn.t_1)
m3 = lm(abreturn.t~.-abreturn.t,data=dat3)
summary(m3)
# which indicates that this model is also a bad one
# no correlation with other vars

# event study
# install.packages('eventstudies',dependencies = T)
library(eventstudies)
library(zoo)
StockAbreturns = zoo(cbind(abreturn), order.by=as.Date(final$date[-n]))
head(StockAbreturns)
str(StockAbreturns)

SplitDates$name = c("abreturn")
head(SplitDates)
str(SplitDates)

es1 = eventstudy(
  firm.returns = StockAbreturns,
  event.list = SplitDates,
  event.window = 5,
  type = "None",
  to.remap = TRUE,
  remap = "cumsum",
  inference = TRUE,
  inference.strategy = "bootstrap")
es1

# return
return = diff(log(final$DJUSIP))

plot(ts(return,start=2001,frequency = 252),xlim=c(2001,2018),axes=F)
axis(1,at=2001:2018,labels=2001:2018);axis(2);box()

# return linear model
n = dim(data.frame(return))[1]
dat4 = cbind(dat[-n,],return)
m4 = lm(return~.-return,data=dat4)
summary(m4)
# no sig

# return vs AR(1)
return.t = return[-n]
return.t_1 = return[-1]
dat5 = cbind(dat[-c(n,n-1),],return.t,return.t_1)
m5 = lm(return.t~.-return.t,data=dat5)
summary(m5)
# AR(1) sig

# event study
StockReturns = zoo(cbind(return), order.by=as.Date(final$date[-n]))
head(StockReturns)
str(StockReturns)

SplitDates$name = c("return")
head(SplitDates)
str(SplitDates)

es2 = eventstudy(
  firm.returns = StockReturns,
  event.list = SplitDates,
  event.window = 10,
  type = "None",
  to.remap = TRUE,
  remap = "cumprod",
  inference = TRUE,
  inference.strategy = "bootstrap")
es2

######
#Price
######

# abnormal price
DJIprice = log(final$DJI)
DJUSIPprice = log(final$DJUSIP)

m = lm(DJUSIPprice~DJIprice)
abprice = DJUSIPprice - m$coefficients[1]-m$coefficients[2]*DJIprice

plot(ts(abprice,start=2001,frequency = 252),xlim=c(2001,2018),axes=F)
axis(1,at=2001:2018,labels=2001:2018);axis(2);box()

# abprice linear model -- could be a good one
dat6 = cbind(dat,abprice)
m6 = lm(abprice~.-abprice,data=dat6)
summary(m6)
m6.step = step(m6,direction = "backward")
summary(m6.step)

# abprice vs AR(1)
n = dim(data.frame(abprice))[1]
abprice.t_1 = abprice[-1]
abprice.t = abprice[-n]

dat7 = cbind(dat[-n,],abprice.t,abprice.t_1)
m7 = lm(abprice.t~.-abprice.t,data=dat7)
summary(m7)
# AR(1) sig
# no correlations

# event study

StockAbprice = zoo(cbind(abprice), order.by=as.Date(final$date))
head(StockAbprice)
str(StockAbprice)

SplitDates$name = c("abprice")
head(SplitDates)
str(SplitDates)


es3 = eventstudy(
  firm.returns = StockAbprice,
  event.list = SplitDates,
  event.window = 10,
  type = "None",
  to.remap = TRUE,
  remap = "cumsum",
  inference = TRUE,
  inference.strategy = "bootstrap")
es3

# price
price = log(final$DJI)
plot(ts(price,start=2001,frequency = 252),xlim=c(2001,2018),axes=F)
axis(1,at=2001:2018,labels=2001:2018);axis(2);box()

# price linear model
dat8 = cbind(dat,price)
m8 = lm(price~.-price,data=dat8)
summary(m8)
# death, temp rise or decrease not sig
m8.step = step(m8, direction="backward")
summary(m8.step)

# price vs AR(1)
n = dim(data.frame(price))[1]
price.t_1 = price[-1]
price.t = price[-n]

dat9 = cbind(dat[-n,],price.t,price.t_1)
m9 = lm(price.t~.-price.t,data=dat9)
summary(m9)
# AR(1) sig
# no correlations

# event study
StockPrice = zoo(cbind(price), order.by=as.Date(final$date))
head(StockPrice)
str(StockPrice)

SplitDates$name = c("price")
head(SplitDates)
str(SplitDates)

es4 = eventstudy(
  firm.returns = StockPrice,
  event.list = SplitDates,
  event.window = 10,
  type = "None",
  to.remap = TRUE,
  remap = "cumsum",
  inference = TRUE,
  inference.strategy = "bootstrap")
es4

#######
#Volume
#######
plot(ts(volume,start=2001,frequency = 252),xlim=c(2001,2018),axes=F)
axis(1,at=2001:2018,labels=2001:2018);axis(2);box()

# volume linear model
dat10 = cbind(volume, dat)
m10 = lm(volume~., data=dat10)
summary(m10)
m10.step = step(m10,direction="backward")
summary(m10.step)

# event study
StockVolume = zoo(cbind(volume), order.by=as.Date(final$date))
head(StockVolume)
str(StockVolume)

SplitDates$name = c("volume")
head(SplitDates)
str(SplitDates)

es5 = eventstudy(
  firm.returns = StockVolume,
  event.list = SplitDates,
  event.window = 5,
  type = "None",
  to.remap = TRUE,
  remap = "cumsum",
  inference = TRUE,
  inference.strategy = "bootstrap")
es5
