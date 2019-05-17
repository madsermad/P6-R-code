# Packages
library(forecast)
library(astsa)
library(readxl)

# Beforehand definitions
Eldaily2013 <- read_excel("Eldaily2014-2018.xlsx", 
                          col_types = c("numeric", "blank", "blank", 
                                        "blank", "blank", "blank", "blank", 
                                        "numeric", "numeric", "blank", "blank", 
                                        "blank", "blank", "blank", "blank", 
                                        "blank", "blank", "blank", "blank")) # Load dataset
colnames(Eldaily2013)[1] <- "Date"
dk1f <- Eldaily2013[1:2]-mean(unlist(Eldaily2013))
dk1f[2] <- dk1f[2]-mean(unlist(dk1f[2])) # We are working with this 


# The actual data for 2019
Eldaily2019 <- read_excel("Eldaily2019.xlsx", 
                          col_types = c("numeric", "blank", "blank", 
                                        "blank", "blank", "blank", "blank", 
                                        "numeric", "numeric", "blank", "blank", 
                                        "blank", "blank", "blank", "blank", 
                                        "blank", "blank", "blank")) # Load dataset
colnames(Eldaily2019)[1] <- "Date"
dk1f19 <- Eldaily2019[1:2]
dk1f19[2] <- dk1f19[2]-mean(unlist(dk1f19[2])) # We are working with this 



# Rolling forecast for AR(7) (1 day forecasting)
h <- 1
train <- window(ts(dk1f[2],end=1825))
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(7,0,0),include.mean = FALSE)
fc1_ar <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

for(i in 1:n){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  fc1_ar[i] <- forecast(refit, h=h)$mean[1]
  foreu[i] <- forecast(refit, h=h)$upper[2]
  forel[i] <- forecast(refit, h=h)$lower[2]
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "1 day forecasting of AR(7)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc1_ar,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for AR(7) (7 days forecasting)
h <- 7
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(7,0,0),include.mean = FALSE)
fc7_ar <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

vec <- c(1)
for (i in 1:20) {
  vec <- c(vec,i+(6*i))
}
for(i in vec){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  for (j in 0:6) {
    fc7_ar[i+j] <- forecast(refit, h=h)$mean[j+1]
  foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
  forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "7 days forecasting of AR(7)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc7_ar,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for AR(7) (14 days forecasting)
h <- 14
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(7,0,0),include.mean = FALSE)
fc14_ar <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)


vec <- c(1)
for (i in 1:10) {
  vec <- c(vec,i+(13*i))
}
for(i in vec){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  for (j in 0:13) {
    fc14_ar[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "14 days forecasting of AR(7)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc14_ar,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for AR(7) (21 days forecasting)
h <- 21
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(7,0,0),include.mean = FALSE)
fc21_ar <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)


vec <- c(1)
for (i in 1:10) {
  vec <- c(vec,i+(20*i))
}
for(i in vec){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  for (j in 0:20) {
    fc21_ar[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "21 days forecasting of AR(7)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc21_ar,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for MA(9) (1 day forecasting)
h <- 1
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(0,0,9),,include.mean = FALSE)
fc1_ma <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)


for(i in 1:n){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  fc1_ma[i] <- forecast(refit, h=h)$mean
  foreu[i] <- forecast(refit, h=h)$upper[2]
  forel[i] <- forecast(refit, h=h)$lower[2]
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "1 day forecasting of MA(9)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc1_ma,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for MA(9) (7 days forecasting)
h <- 7
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(0,0,9),include.mean = FALSE)
fc7_ma <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

vec <- c(1)
for (i in 1:20) {
  vec <- c(vec,i+(6*i))
}
for(i in vec){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  for (j in 0:6) {
    fc7_ma[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "7 days forecasting of MA(9)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc7_ma,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for MA(9) (14 days forecasting)
h <- 14
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(0,0,9),include.mean = FALSE)
fc14_ma <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

vec <- c(1)
for (i in 1:10) {
  vec <- c(vec,i+(13*i))
}
for(i in vec){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  for (j in 0:13) {
    fc14_ma[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "14 days forecasting of MA(9)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc14_ma,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for MA(9) (21 days forecasting)
h <- 21
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(0,0,9),include.mean = FALSE)
fc21_ma <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

vec <- c(1)
for (i in 1:10) {
  vec <- c(vec,i+(20*i))
}
for(i in vec){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  for (j in 0:20) {
    fc21_ma[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "21 days forecasting of MA(9)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc21_ma,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for ARMA(2,2) (1 day forecasting)
h <- 1
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(2,0,2),include.mean = FALSE)
fc1_arma <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

for(i in 1:n){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  fc1_arma[i] <- forecast(refit, h=h)$mean
  foreu[i] <- forecast(refit, h=h)$upper[2]
  forel[i] <- forecast(refit, h=h)$lower[2]
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "1 day forecasting of ARMA(2,2)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc1_arma,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for ARMA(2,2) (7 days forecasting)
h <- 7
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(2,0,2),include.mean = FALSE)
fc7_arma <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

vec <- c(1)
for (i in 1:20) {
  vec <- c(vec,i+(6*i))
}
for(i in vec){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  for (j in 0:6) {
    fc7_arma[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "7 days forecasting of ARMA(2,2)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc7_arma,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for ARMA(2,2) (14 days forecasting)
h <- 14
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(2,0,2),include.mean = FALSE)
fc14_arma <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

vec <- c(1)
for (i in 1:10) {
  vec <- c(vec,i+(13*i))
}
for(i in vec){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  for (j in 0:13) {
    fc14_arma[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "14 days forecasting of ARMA(2,2)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc14_arma,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# Rolling forecast for ARMA(2,2) (21 days forecasting)
h <- 21
train <- window(ts(dk1f[2]),end=1825)
test <- window(c(rep(NA,1825),ts(dk1f19[2])),start=1826)
n <- length(test)
fit <- arima(train,c(2,0,2),include.mean = FALSE)
fc21_arma <- ts(numeric(n), start=1826)
foreu <- ts(numeric(n), start=1826)
forel <- ts(numeric(n), start=1826)

vec <- c(1)
for (i in 1:10) {
  vec <- c(vec,i+(20*i))
}
for(i in vec){  
  x <- window(c(ts(dk1f[2]),ts(dk1f19[2])), end=1825 + (i-1),start = 1)
  refit <- Arima(x, model=fit)
  for (j in 0:20) {
    fc21_arma[i+j] <- forecast(refit, h=h)$mean[j+1]
    foreu[i+j] <- forecast(refit, h=h)$upper[j+1,2]
    forel[i+j] <- forecast(refit, h=h)$lower[j+1,2]
  }
}
ts.plot(dk1f[2],xlim = c(1800,1950),ylim = c(-400,400), ylab = "Prices", main = "21 days forecasting of ARMA(2,2)")
x1 <- c(1826:1947)
y1 <- c(forel)
y2 <- c(foreu)
polygon(c(x1,rev(x1)),c(y2,rev(y1)),col="grey", border = "White")
lines(fc21_arma,col = "red")
lines(c(rep(NA,1825),ts(dk1f19[,2])))
segments(1825, unlist(dk1f[1825,2]), x1 = 1826, y1 = unlist(dk1f19[1,2]))

# RMSE
library(Metrics)
actual <- c(unlist(dk1f19[2]))

# AR(7) | MA(9) | ARMA(2,2) for 1 day
rmse(actual, c(fc1_ar));rmse(actual, c(fc1_ma));rmse(actual, c(fc1_arma))

# AR(7) | MA(9) | ARMA(2,2) for 7 days
rmse(actual, c(fc7_ar));rmse(actual, c(fc7_ma));rmse(actual, c(fc7_arma)) 

# AR(7) | MA(9) | ARMA(2,2) for 14 days
rmse(actual, c(fc14_ar));rmse(actual, c(fc14_ma));rmse(actual, c(fc14_arma))

# AR(7) | MA(9) | ARMA(2,2) for 21 days
rmse(actual, c(fc21_ar));rmse(actual, c(fc21_ma));rmse(actual, c(fc21_arma))





