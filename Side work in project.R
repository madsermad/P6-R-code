### Packages 
library(tseries)
library(readxl)
library(astsa)
library(fracdiff)
library(forecast)


### Beforehand definitions
Eldaily2014_18 <- read_excel("Eldaily2014-2018.xlsx", 
                          col_types = c("numeric", "blank", "blank", 
                                        "blank", "blank", "blank", "blank", 
                                        "numeric", "numeric", "blank", "blank", 
                                        "blank", "blank", "blank", "blank", 
                                        "blank", "blank", "blank", "blank")) # Load dataset
colnames(Eldaily2014_18)[1] <- "Date"
dk1f <- Eldaily2014_18[1:2] 
dk1f[2] <- dk1f[2]-mean(unlist(dk1f[2])) # We are working with this



### Figure 2.1 and Figure 3.2
# Process to add date to data.
data <- as.Date(as.Date("2014-01-01"):as.Date("2018-12-31"), origin="1970-01-01")
which(data == as.Date("2016-02-29"))
mydata <- c()
for (i in 1:1826) {
  if (!(i == 790) && i<=790) {
    mydata[i] <- as.Date(data[i])
  }
  if (i>790) {
    mydata[i-1] <- as.Date(data[i])
  }
  class(mydata) <- "Date"
}
Eldaily2014_18$Date <- mydata
dk1f <- Eldaily2014_18[1:2]
dk1f[2] <- dk1f[2]-mean(unlist(dk1f[2]))
plot(dk1f,xlab = "Time", ylab = "Elspot prices", main = "Elspot prices from 2014-2018", type = "l")

### Figure 3.3
z <- decompose(ts(dk1f[2],frequency = 30.41667))
par(mfrow = c(2,1)) # Split the graphs
plot(z$x, main = "Observations", xlab = "Months", ylab = "Prices")
plot(z$trend, main = "Trend", xlab = "Months", ylab = "Prices")
par(mfrow = c(1,1)) # Reset the splitting

### Figure 3.4 
d <- decompose(ts(dk1f[1:(365/2),2],frequency = 30.41667))
par(mfrow = c(2,1)) # Split the graphs
plot(d$x, main = "Observations", xlab = "Months", ylab = "Prices")
plot(d$seasonal, main = "Seasonal", xlab = "Months", ylab = "Prices")
par(mfrow = c(1,1)) # Reset the splitting

### Figure 4.1 
acf1(ts(dk1f[2]), main = "ACF for DK1")

### Dickey - Fuller in Table 4.2 
adf.test(ts(dk1f[2],frequency = 7)) 
# Change of frequency gives the same result

### Figure 4.2
set.seed(10)
n <- 1000 # Define number of observations.
x <- cumsum(sample(c(-1, 1), n, TRUE))
acf1(x, main = "ACF for random walk")

# PACF for DK1 (figure 5.3)
pacf(dk1f[2])






