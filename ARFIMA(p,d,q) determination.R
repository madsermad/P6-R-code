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

frac_d <- fracdiff(x = ts(dk1f[2]))$d # Fractional difference
fdiff <- diffseries(x = ts(dk1f[2]), d = frac_d)


# Order of p and q according to AIC
pai1 <- matrix(ncol = 7, nrow = 7)
for (q in 0:6) {
  for (p in 0:6) {
    pai1[p+1,q+1] <- (arima(fdiff,c(p,0,q),method = "ML",include.mean = FALSE))$aic
    print(p)
  }
  print(100+q)
}
min(pai1,na.rm = TRUE); pai1 # finder en p = 5 og en q = 6

# Order of p and q according to BIC
pai2 <- matrix(ncol = 7, nrow = 7)
for (q in 0:6) {
  for (p in 0:6) {
    pai2[p+1,q+1] <- BIC(arima(fdiff,c(p,0,q),method = "ML",include.mean = FALSE))
    print(p)
  }
  print(100+q)
}
min(pai2,na.rm = TRUE); pai2 # finder en p = 5 og en q = 6

# Plot
plot(c(pai1),type = "l",ylab= "AIC and BIC values",xlab = "Order of p"); abline(v = 8, col=1, lwd=1, lty=2);abline(v = 15, col=1, lwd=1, lty=2);abline(v = 22, col=1, lwd=1, lty=2); abline(v = 29, col=1, lwd=1, lty=2);abline(v = 36, col=1, lwd=1, lty=2);abline(v = 43, col=1, lwd=1, lty=2)
lines(c(pai2),col = "red")
# Minimum points
points(19, c(pai1[19])); points(19,c(pai2[19]),col = "red"); points(27,c(pai1[27]));points(27,c(pai2[27]),col = "red");points(35,c(pai1[35]));points(35,c(pai2[35]),col = "red");points(42,c(pai1[42]));points(42,c(pai2[42]),col = "red");points(48,c(pai1[48]));points(48,c(pai2[48]),col = "red");
text(locator(), labels = c("q = 0", "q = 1","q = 2","q = 3","q = 4","q = 5","q = 6"))

# Estimation of ARFIMA(p,d,q)
arima(fdiff,c(4,0,2), include.mean = FALSE)

# Residual analysis
sarima(fdiff,4,0,2,no.constant = TRUE)
