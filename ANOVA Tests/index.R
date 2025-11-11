# installing packages
install.packages("TTR")
install.packages("forecast")

# calling the libraries
library(TTR)
library(forecast)

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings

# store the variable as time series object
kingstimeseries <- ts(kings)
kingstimeseries

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat") 
birthstimeseries <- ts(births, frequency = 12, start = c(1946, 1))
birthstimeseries

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") 
souvenirtimeseries <- ts(souvenir, frequency = 12, start = c(1987, 1))
souvenirtimeseries

# plotting time series
plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)

# log-transform timeseries
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

# smoothing time series
kingstimeseriesSMA3 <- SMA(kingstimeseries, n=3)
plot.ts(kingstimeseriesSMA3)

#smoothing with higher order
kingstimeseriesSMA8 <- SMA(kingstimeseries, n=8)
plot.ts(kingstimeseriesSMA8)

birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal
plot(birthstimeseriescomponents)

# seasonally adjusting
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal

plot(birthstimeseriesseasonallyadjusted)

# rain data
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1) 
rainseries <- ts(rain, start = c(1813))
plot.ts(rainseries)

#simple exponential smoothing
rainseriesforecast <- HoltWinters(rainseries, beta = FALSE, gamma = FALSE)
rainseriesforecast

rainseriesforecast$fitted
plot(rainseriesforecast)
rainseriesforecast$SSE

HoltWinters(rainseries, beta=FALSE, gamma = FALSE, l.start =23.56)

rainseriesforecast2 <- forecast(rainseriesforecast, h=8)
rainseriesforecast2
plot(rainseriesforecast2)

# correllogram
acf(rainseriesforecast2$residuals, lag.max = 20, na.action = na.pass)

# test
Box.test(rainseriesforecast2$residuals, lag = 20, type = "Ljung-Box")
plot.ts(rainseriesforecast2$residuals)

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4 
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

rainseriesforecast2$residuals <- rainseriesforecast2$residuals[!is.na(rainseriesforecast2$residuals)]
plotForecastErrors(rainseriesforecast2$residuals)

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5) 
skirtsseries <- ts(skirts, start = c(1866))
plot.ts(skirtsseries)

skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts

skirtsseriesforecasts$SSE
plot(skirtsseriesforecasts)

#Holtwinters
HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9)

skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)
plot(skirtsseriesforecasts2)
acf(na.omit(skirtsseriesforecasts2$residuals), lag.max=20)
Box.test(na.omit(skirtsseriesforecasts2$residuals), lag=20, type="Ljung-Box")
plot.ts(skirtsseriesforecasts2$residuals)
skirtsseriesforecasts2$residuals <- skirtsseriesforecasts2$residuals[!is.na(skirtsseriesforecasts2$residuals)]
plotForecastErrors(skirtsseriesforecasts2$residuals)


logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts
souvenirtimeseriesforecasts$SSE
plot(souvenirtimeseriesforecasts)


souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts,h=48)
plot(souvenirtimeseriesforecasts2)
acf(souvenirtimeseriesforecasts2$residuals, lag.max=20 , na.action = na.pass)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")
