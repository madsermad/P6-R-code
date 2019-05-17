### Packages 
library(tseries)
library(readxl)
library(astsa)
library(fracdiff)

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

### Figure 4.5 
aic <- c()
for (i in 0:12) { # Calculating aic
  aic[i] <- arima(ts(dk1f[2],frequency = 7),c(0,0,i),include.mean = FALSE)$aic
}
bic <- c()
for (i in 0:12) { # Calculating bic
  bic[i] <- BIC(arima(dk1f[2],c(0,0,i),include.mean = FALSE))
}
plot(aic, type = "l", ylab = "AIC and BIC values", xlab = "Order of q")
lines(bic, col = "red")
points(12,aic[12]); points(12, bic[12]); points(9,aic[9]); points(9,bic[9]) # The highlighted values.
text(locator(), labels = c("AIC", "BIC"))

### Estimate the MA(9) model
arima(ts(dk1f[2]),c(0,0,9),include.mean = FALSE) # MA(9)

### Residual Analysis of MA(9) (figure 4.6)
sarima(ts(dk1f[2]),0,0,9, no.constant = TRUE)
