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

### Figure 4.3 
aic <- c() 
for (i in 0:12) { # Calculating the AIC for each lag
  aic[i] <- arima(ts(dk1f[2],frequency = 7),c(i,0,0),include.mean = FALSE)$aic
}
bic <- c()
for (i in 0:12) { # Calculating the BIC for each lag
  bic[i] <- BIC(arima(dk1f[2],c(i,0,0),include.mean = FALSE))
}
plot(aic, type = "l", ylab = "AIC and BIC values", xlab = "Order of p", ylim = c(19400,19650)) # The AIC
lines(bic, col = "red") # The BIC
points(7,min(bic)); points(7,aic[7]); points(12,aic[12]) # Making minimum points
text(locator(), labels = c("AIC", "BIC")) # Press the graph to add AIC and BIC, then press esc.

# Estimation of an AR(7) model
arima(ts(dk1f[2]),c(7,0,0),include.mean = FALSE)

# Residual Analysis of an AR(7) model
sarima(ts(dk1f[2]),7,0,0,no.constant = TRUE)
