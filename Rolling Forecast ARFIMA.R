library(forecast)
library(astsa)
library(readxl)

setwd("C:/Users/mads/Dropbox/P6/Data/Elspot prices")

Eldaily2013 <- read_excel("Eldaily2014-2018.xlsx", 
                          col_types = c("numeric", "blank", "blank", 
                                        "blank", "blank", "blank", "blank", 
                                        "numeric", "numeric", "blank", "blank", 
                                        "blank", "blank", "blank", "blank", 
                                        "blank", "blank", "blank", "blank"))
colnames(Eldaily2013)[1] <- "Date"
dk1f <- Eldaily2013[1:2]-mean(unlist(Eldaily2013))
dk1f[2] <- dk1f[2]-mean(unlist(dk1f[2]))

Eldaily2019 <- read_excel("Eldaily2019.xlsx", 
                          col_types = c("numeric", "blank", "blank", 
                                        "blank", "blank", "blank", "blank", 
                                        "numeric", "numeric", "blank", "blank", 
                                        "blank", "blank", "blank", "blank", 
                                        "blank", "blank", "blank"))
colnames(Eldaily2019)[1] <- "Date"
dk1f19 <- Eldaily2019[1:2]
dk1f19[2] <- dk1f19[2]-mean(unlist(dk1f19[2]))
d <- fracdiff(ts(dk1f[2]))$d
fdiff <- diffseries(ts(dk1f[2]),d)
fdiff19 <- diffseries(ts(dk1f19[2]),d)

# Rolling forecast for ARFIMA (1 day forecasting)
h <- 1
train <- window(ts(fdiff,frequency = 7),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2],frequency = 7)),start=1826)
n <- length(test)
fit <- arima(train,c(4,0,2), include.mean = FALSE,method = "ML")
fc1_arf <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

for(i in 1:n){  
  x <- window(c(ts(fdiff,frequency = 7),ts(dk1f19[2],frequency = 7)), end=1825 + (i-1),start = 1 + (i-1))
  refit <- Arima(x, model=fit)
  fc1_arf[i] <- forecast(refit, h=h)$mean[1]
  foreu[i] <- forecast(refit, h=h)$upper[2]
  forel[i] <- forecast(refit, h=h)$lower[2]
}
ts.plot(fdiff,xlim = c(1800,1950), ylab = "Prices", main = "1 day forecasting of ARFIMA",ylim = c(-400,400))
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc1_arf,col = "red")
lines(c(rep(NA,1825),ts(fdiff19)))
segments(1825, fdiff[1825], x1 = 1826, y1 = fdiff19[1])

# Rolling forecast for ARFIMA (7 days forecasting)
h <- 7
train <- window(ts(fdiff, frequency = 7),end=1825)
test <- window(c(rep(NA,1825),ts(fdiff19,frequency = 7)),start=1826)
n <- length(test)
fit <- arima(train,c(4,0,2), include.mean = FALSE,method = "ML")
fc7_arf <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

vec <- c(1)
for (i in 1:20) {
  vec <- c(vec,i+(6*i))
}
for(i in vec){  
  x <- window(c(ts(fdiff,frequency = 7),ts(fdiff19,frequency = 7)), end=1825 + (i-1),start = 1+ (i-1))
  refit <- Arima(x, model=fit)
  for (j in 0:6) {
    fc7_arf[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(fdiff,xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "7 days forecasting of ARFIMA")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc7_arf,col = "red")
lines(c(rep(NA,1825),ts(fdiff19)))
segments(1825, fdiff[1825], x1 = 1826, y1 = fdiff19[1])


# Rolling forecast for ARFIMA (14 days forecasting)
h <- 14
train <- window(ts(fdiff,frequency = 7),end=1825)
test <- window(c(rep(NA,1825),ts(fdiff19,frequency = 7)),start=1826)
n <- length(test)
fit <- arima(train,c(4,0,2), include.mean = FALSE,method = "ML")
fc14_arf <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

vec <- c(1)
for (i in 1:20) {
  vec <- c(vec,i+(13*i))
}
for(i in vec){  
  x <- window(c(ts(fdiff,frequency = 7),ts(fdiff19,frequency = 7)), end=1825 + (i-1),start = 1+ (i-1))
  refit <- Arima(x, model=fit)
  for (j in 0:13) {
    fc14_arf[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(fdiff,xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "14 days forecasting of ARFIMA")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc14_arf,col = "red")
lines(c(rep(NA,1825),ts(fdiff19)))
segments(1825, fdiff[1825], x1 = 1826, y1 = fdiff19[1])

# Rolling forecast for ARFIMA (21 days forecasting)
h <- 21
train <- window(ts(fdiff,frequency = 7),end=1825)
test <- window(c(rep(NA,1825),ts(fdiff19,frequency = 7)),start=1826)
n <- length(test)
fit <- arima(train,c(4,0,2), include.mean = FALSE,method = "ML")
fc21_arf <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

vec <- c(1)
for (i in 1:20) {
  vec <- c(vec,i+(20*i))
}
for(i in vec){  
  x <- window(c(ts(fdiff,frequency = 7),ts(fdiff19,frequency = 7)), end=1825 + (i-1),start = 1+ (i-1))
  refit <- Arima(x, model=fit)
  for (j in 0:20) {
    fc21_arf[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(fdiff,xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "21 days forecasting of ARFIMA")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc21_arf,col = "red")
lines(c(rep(NA,1825),ts(fdiff19)))
segments(1825, fdiff[1825], x1 = 1826, y1 = fdiff19[1])

library(Metrics)
rmse(fdiff19, c(fc1_arf));rmse(fdiff19, c(fc7_arf));rmse(fdiff19, c(fc14_arf));rmse(fdiff19, c(fc21_arf))
