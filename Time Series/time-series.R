# Installing Packages
install.packages("TTR")
install.packages("forecast")

# Calling the installed packages (library)
library(TTR)
library(forecast)

# Skirts series
skirts <- read.csv("skirts.dat")
skirtsseries <- ts(skirts, start=c(1866))
plot.ts(skirtsseries)
skirtseriesdiff1 <- diff(skirtsseries, difference=1)
plot.ts(skirtseriesdiff1)

skirtseriesdiff2 <- diff(skirtsseries, differences = 2)
plot.ts(skirtseriesdiff2)

#Kings data
kings <- read.csv("kings.dat")
kingstimeseries <- ts(kings)
plot.ts(kingstimeseries)                      
kingstimeseriesdiff1 <- diff(kingstimeseries, difference=1)
plot.ts(kingstimeseriesdiff1)

# plotting correlogram
acf(kingstimeseriesdiff1, lag.max = 20)
acf(kingstimeseriesdiff1, lag.max = 20, plot = FALSE)

Pacf(kingstimeseriesdiff1, lag.max = 20)
pacf(kingstimeseriesdiff1, lag.max = 20, plot = FALSE)

volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)

acf(volcanodustseries, lag.max=20) 
acf(volcanodustseries, lag.max=20, plot=FALSE)



pacf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20, plot=FALSE)

kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
kingstimeseriesarima

library("forecast") # load the "forecast" R library
kingstimeseriesforecasts <- forecast(kingstimeseriesarima, h=5)
kingstimeseriesforecasts
plot(kingstimeseriesforecasts)

acf(kingstimeseriesforecasts$residuals, lag.max=20)
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box")
plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation
  # equal to the standard deviation of the forecast errors:
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a histogram of the forecast errors:
  hist(forecasterrors, col="red", freq=FALSE, 
       xlim=c(mymin, mymax), breaks=mybinsize, 
       main="Forecast Errors Histogram")
  # plot a normal distribution curve on top:
  lines(density(mynorm), col="blue", lwd=2)
}

plot.ts(kingstimeseriesforecasts$residuals) # time plot forecast erro
plotForecastErrors(kingstimeseriesforecasts$residuals)

volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesarima


volcanodustseriesforecasts <- forecast(volcanodustseriesarima, h=31)
volcanodustseriesforecasts
plot(volcanodustseriesforecasts)

acf(volcanodustseriesforecasts$residuals, lag.max=20)
Box.test(volcanodustseriesforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(volcanodustseriesforecasts$residuals)
plotForecastErrors(volcanodustseriesforecasts$residuals) 

plot.ts(volcanodustseriesforecasts$residuals)
plotForecastErrors(volcanodustseriesforecasts$residuals) # make a histogra

mean(volcanodustseriesforecasts$residuals)









