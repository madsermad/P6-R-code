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

# Figure 4.7 
aic1 <- matrix(ncol = 7, nrow = 7)
for (q in 0:6) { 
  for (p in 0:6) {
    c <- c()
    c <- arima(ts(dk1f[2],frequency = 7),c(p,0,q),include.mean = FALSE, method = "ML",optim.method="Nelder-Mead")$aic
    aic1[1+p,1+q] <- c
    print("1")
  }
  print("2")
}  
### When aic1 is found, the bic is found using the same method.
bic1 <- matrix(ncol = 7, nrow = 7)
for (q in 0:6) { 
  for (p in 0:6) {
    c <- c()
    c <- BIC(arima(ts(dk1f[2],frequency = 7),c(p,0,q),include.mean = FALSE, method = "ML",optim.method="Nelder-Mead"))
    bic1[1+p,1+q] <- c
    print("1")
  }
  print("2")
}
## Now this can be plotted
plot(c(aic1), type = "l", ylab = "AIC and BIC", xlab = "") # Plotting of AIC values
points(7,aic1[7,1]);points(11,aic1[11-7,2]); points(17,aic1[17-14,3]); points(31,aic1[31-28,5]); points(38,aic1[38-35,6]); points(46,aic1[46-42,7]) # Small AIC values
abline(v = 8, col=1, lwd=1, lty=2);abline(v = 15, col=1, lwd=1, lty=2);abline(v = 22, col=1, lwd=1, lty=2); abline(v = 29, col=1, lwd=1, lty=2);abline(v = 36, col=1, lwd=1, lty=2);abline(v = 43, col=1, lwd=1, lty=2)

lines(c(bic1),type = "l", col = "red") # Adding of BIC values
points(7,bic1[7,1],col = "red");points(11,bic1[11-7,2],col = "red"); points(17,bic1[17-14,3],col = "red"); points(31,bic1[31-28,5],col = "red"); points(38,bic1[38-35,6],col = "red"); points(46,bic1[46-42,7],col = "red") # Small BIC values
text(locator(), labels = c("q = 0", "q = 1","q = 2","q = 3","q = 4","q = 5","q = 6")) # Add q values areal. 
### Estimating the different arma models 
arima(ts(dk1f[2],frequency = 7),c(6,0,0),include.mean = FALSE, method = "ML") # ARMA(6,0)
arima(ts(dk1f[2],frequency = 7),c(3,0,1),include.mean = FALSE, method = "ML") # ARMA(3,1)
arima(ts(dk1f[2],frequency = 7),c(2,0,2),include.mean = FALSE, method = "ML") # ARMA(2,2)
arima(ts(dk1f[2],frequency = 7),c(4,0,6),include.mean = FALSE, method = "ML") # ARMA(4,6)

### Residual analysis of ARMA(2,2)
# Source the mysarima 
mysarima(ts(dk1f[2],frequency = 7),2,0,2,no.constant = TRUE) # This has method = ML