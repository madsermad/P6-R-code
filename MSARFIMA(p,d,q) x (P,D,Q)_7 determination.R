
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
dk1f_frac_diff <- diffseries(x = ts(dk1f[2]), d = frac_d)

min_bic = Inf
for (p in 0:4){
  for(q in 0:4){
    for(P in 0:2){
      for(Q in 0:3){
        ts = Arima(ts(dk1f_frac_diff,frequency = 7),order=c(p,0,q),
                   seasonal = list(order=c(P,1,Q)), method="ML",include.mean = FALSE)
        cat("Model",p,0,q,P,1,Q,"bic",ts$bic,"\n")
        if (ts$bic < min_bic){
          min_bic = ts$bic
          best_fit = ts 
        }
      }
    }
  }
}

# Estimation of MSARFIMA(p,d,q) x (P,D,Q)_s
print(best_fit)

# Residual analysis 
sarima(ts(dk1f_frac_diff,frequency = 7),1,0,2,0,1,1,7,no.constant = TRUE)





