setwd('D:\\Project\\OR538 Project\\dataset')

#===============================#
#      DJUSIP Analysis          #
#===============================#

# data loading

DJI = read.csv('^DJI.csv', header = T)
DJUSIP = read.csv('^DJUSIP.csv', header = T)
final = read.csv('final_dataset.csv', header = T)

# abnormal return
DJIreturn = diff(log(final$DJI))
DJUSIPreturn = diff(log(final$DJUSIP))

m = lm(DJUSIPreturn~DJIreturn)
abreturn = DJUSIPreturn - m$coefficients[1]-m$coefficients[2]*DJIreturn

plot(ts(abreturn,start=2001,frequency = 252),xlim=c(2001,2018),axes=F)
axis(1,at=2001:2018,labels=2001:2018);axis(2);box()

acf(abreturn)
pacf(abreturn)

library(forecast)
m1 = auto.arima(abreturn)
summary(m1)

n = dim(data.frame(abreturn))[1]
abreturn.t_1 = abreturn[-1]
abreturn.t_2 = abreturn.t_1[-1]
abreturn.t = abreturn[-n]

m2= lm(abreturn.t[-(n-1)]~abreturn.t_1[-(n-1)]+abreturn.t_2)
summary(m2)

final$During = as.character(final$During)
final$Strong = as.character(final$Strong)
final$Death = as.character(final$Death)
final$temp_rise = as.character(final$temp_rise)
final$temp_decrease = as.character(final$temp_decrease)
final$high_temp = as.character(final$high_temp)
dat = final[,-c(1,12,13,14)]
volume = final[,14]

dat2 = cbind(dat[-c(4208,4209,4210),],abreturn.t[-(n-1)],abreturn.t_1[-(n-1)],abreturn.t_2)
colnames(dat2) = c('avgTemp','maxTemp','minTemp','During','Strong','Death','temp_rise',
                   'temp_decrease','high_temp','temp_diff','abreturn.t','abreturn.t_1',
                   'abreturn.t_2')
m3 = lm(abreturn.t~.-abreturn.t,data=dat2)
summary(m3)
# which indicates that I use abnormal return
# is a very bad choice to invest the correlations

# abnormal price

DJIprice = log(final$DJI)
DJUSIPprice = log(final$DJUSIP)

m = lm(DJUSIPprice~DJIprice)
abprice = DJUSIPprice - m$coefficients[1]-m$coefficients[2]*DJIprice

plot(ts(abprice,start=2001,frequency = 252),xlim=c(2001,2018),axes=F)
axis(1,at=2001:2018,labels=2001:2018);axis(2);box()

acf(abprice)
acf(diff(abprice))
pacf(abprice)

library(forecast)
m1 = auto.arima(abprice)
summary(m1)

n = dim(data.frame(abprice))[1]
abprice.t_1 = abprice[-1]
abprice.t = abprice[-n]

m2= lm(abprice.t~abprice.t_1)
summary(m2)

final$During = as.character(final$During)
final$Strong = as.character(final$Strong)
final$Death = as.character(final$Death)
final$temp_rise = as.character(final$temp_rise)
final$temp_decrease = as.character(final$temp_decrease)
final$high_temp = as.character(final$high_temp)
dat = final[,-c(1,12,13,14)]
volume = final[,14]

dat2 = cbind(dat[-4210,],abprice.t,abprice.t_1)
colnames(dat2) = c('avgTemp','maxTemp','minTemp','During','Strong','Death','temp_rise',
                   'temp_decrease','high_temp','temp_diff','abprice.t','abprice.t_1')
m3 = lm(abprice.t~.-abprice.t,data=dat2)
summary(m3)

# still not good to see correlations

# return
return = diff(log(final$DJUSIP))

plot(ts(return,start=2001,frequency = 252),xlim=c(2001,2018),axes=F)
axis(1,at=2001:2018,labels=2001:2018);axis(2);box()

acf(return)
pacf(return)

library(forecast)
m1 = auto.arima(return)
summary(m1)

n = dim(data.frame(return))[1]
return.t_1 = return[-1]
return.t_2 = return.t_1[-1]
return.t = return[-n]

m2= lm(return.t[-(n-1)]~return.t_1[-(n-1)]+return.t_2)
summary(m2)

final$During = as.character(final$During)
final$Strong = as.character(final$Strong)
final$Death = as.character(final$Death)
final$temp_rise = as.character(final$temp_rise)
final$temp_decrease = as.character(final$temp_decrease)
final$high_temp = as.character(final$high_temp)
dat = final[,-c(1,12,13,14)]
volume = final[,14]

dat2 = cbind(dat[-c(4208,4209,4210),],return.t[-(n-1)],return.t_1[-(n-1)],return.t_2)
colnames(dat2) = c('avgTemp','maxTemp','minTemp','During','Strong','Death','temp_rise',
                   'temp_decrease','high_temp','temp_diff','return.t','return.t_1',
                   'return.t_2')
m3 = lm(return.t~.-return.t,data=dat2)
summary(m3)

# indicates if temperature is greater than 65, it will decrease the log return

# price
price = log(final$DJI)
plot(ts(price,start=2001,frequency = 252),xlim=c(2001,2018),axes=F)
axis(1,at=2001:2018,labels=2001:2018);axis(2);box()

acf(price)
acf(diff(price))
pacf(price)

library(forecast)
m1 = auto.arima(price)
summary(m1)

n = dim(data.frame(price))[1]
price.t_1 = price[-1]
price.t = price[-n]

m2= lm(price.t~price.t_1)
summary(m2)

final$During = as.character(final$During)
final$Strong = as.character(final$Strong)
final$Death = as.character(final$Death)
final$temp_rise = as.character(final$temp_rise)
final$temp_decrease = as.character(final$temp_decrease)
final$high_temp = as.character(final$high_temp)
dat = final[,-c(1,12,13,14)]
volume = final[,14]

dat2 = cbind(dat[-4210,],price.t,price.t_1)
colnames(dat2) = c('avgTemp','maxTemp','minTemp','During','Strong','Death','temp_rise',
                   'temp_decrease','high_temp','temp_diff','price.t','price.t_1')
m3 = lm(price.t~.-price.t,data=dat2)
summary(m3)

# if I do not include AR term
# tbc