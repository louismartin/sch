filepath <- "../data/Data_centrale_fed_model_vf.csv"

df <- read.csv(filepath, sep=";", dec = ",")

# Remove empty rows
df[df==""] <- NA
df <- na.omit(df)

# Plot PER & TX10
ymax <- max(max(df$PER),max(df$TX10))
plot(c(1:nrow(df)), df$PER, type = 'l', ylim = c(0, ymax), col = 'red', 
     main = "Evolution de PER et TX10 en fonction du temps",
     ylab = '',
     xlab = 'Index temporel')
lines(c(1:nrow(df)), df$TX10, ylim = c(0, ymax), col = 'blue')
legend("topright",legend = c('PER', 'TX10'), col = c('red', 'blue'), lwd = c(1,1))

# Plot PER / TX10
plot(df$TX10, df$PER, xlab = 'TX10', ylab = 'PER')

# Quantiles de l'endogene PER
qqnorm(y=df$PER)

# Distribution empirique de l'endogene TX10
hist(df$PER, nclass = 50)

# Estimation par MCO
reg <- lm(PER~TX10, df)
summary(reg)


## Partie 2 ------------------
# ----------------------------

# Taux d'interet reel
df$TR <- df$TX10-df$CPI

# Regression avec le taux d'interet reel
regtr <- lm(PER~TR, df)
summary(regtr)

# RMSE
rmse <- sqrt(sum(reg$residuals^2)/nrow(df))
rmse_tr <- sqrt(sum(regtr$residuals^2)/nrow(df))

# MAE
mae <- sum(abs(reg$residuals))/nrow(df)
mae_tr <- sum(abs(regtr$residuals))/nrow(df)

# Question 7

dmTest <- function(h){
    # Differences d'erreurs de prediction
    d <- c()
    for (t in 2:(nrow(df)-h)){
        prior <-  df[1:t,] 
        reg <- lm(PER~TX10, prior)
        regtr <- lm(PER~TR, prior)
        pred <- predict(reg, newdata = df[t+h,])
        predtr <- predict(regtr, newdata = df[t+h,])
        real <- df$PER[t+h]
        d[t-1] <-  (real-pred)^2 - (real-predtr)^2
    }
    dbar <-  mean(d)
    
    # Autocovariance empirique des differences
    gamma <- c()
    m <- length(d)
    for (j in 1:m){
        gamma[j] <- sum((d[j:m]-dbar)*(d[1:(m-j+1)]-dbar))/m
    }
    
    # Statistique du test de Diebold et Mariano
    w <-gamma[1] +2*sum(gamma[2:m])
    sdm = dbar / sqrt(abs(w))
    
    print(paste("|SDM| :", abs(sdm)))
    if (abs(sdm)>1.96) print("Hypothese nulle rejetee au seuil de 5%")
    else print("Hypothese nulle non rejetee au seuil de 5%")
}
dmTest(3)

# Question 8.a
beta_estimation <- function(window=5) {
  # Estimation de la stabilite de beta par moyenne glissante
  nrows <- nrow(df)
  n_windows = nrows%/%window
  betas = rep(0, n_windows)
  confints = matrix(0, n_windows, 2)
  for (i in 1:(n_windows)) {
    start <- (i-1)*window
    end <- min((i)*window, nrows)
    reg <- lm(PER~TR, df[start:end, ])
    betas[i] <- reg$coefficients[2]
    confints[i, ] <- c(confint(reg, 'TR', level=0.95))
  }
  ymin = min(c(betas, confints))
  ymax = max(c(betas, confints))
  plot(betas, ylim = c(ymin, ymax),
       main = "Estimation de beta par moyenne glissante")
  lines(confints[, 1], ylim = c(ymin, ymax))
  lines(confints[, 2], ylim = c(ymin, ymax))
}
beta_estimation(10)

# Question 8.b
require("strucchange")
fluct <- efp(PER~TR, type="Rec-CUSUM", data=df)
plot(fluct)

# Question 9
acf(regtr$residuals)
pacf(regtr$residuals)
# La lente decroissance des autocorrelations semble indiquer une tendance non eliminee.
# Avec l'autocorrelation partielle en revanche, on ne note pas d'autocorrelation avec lag
