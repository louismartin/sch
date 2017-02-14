filepath <- "../data/Data_centrale_fed_model_vf.csv"
df <- read.csv(filepath, sep=";", dec = ",")

# Remove empty rows
df[df==""] <- NA
df <- na.omit(df)

# Plot TER


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

# Quantiles de l'endog??ne TX10
qqnorm(y=df$TX10)

# Distribution empirique de l'endog??ne TX10
hist(df$TX10, nclass = 50)

# Estimation par MCO
reg <- lm(PER~TX10, df)
summary(reg)


## Partie 2 ------------------
# ----------------------------

# Taux d'int??r??t r??el
df$TR <- df$TX10-df$CPI

# R??gression avec le taux d'int??r??t r??el
regtr <- lm(PER~TR, df)
summary(regtr)

# RMSE
rmse <- sqrt(sum(reg$residuals^2)/nrow(df))
rmse_tr <- sqrt(sum(regtr$residuals^2)/nrow(df))

# MAE
mae <- sum(abs(reg$residuals))/nrow(df)
mae_tr <- sum(abs(regtr$residuals))/nrow(df)

# Question 7

# Question 8
# Estimation de la stabilite de beta par moyenne glissante
nrows <- nrow(df)
window <- 50
n_windows = nrows%/%window
betas = c()
confints = matrix(0, n_windows, 2)
for (i in 1:n_windows+1) {
  start <- (i-1)*window
  end <- min((i)*window, nrows)
  print(end)
  reg <- lm(PER~TR, df[start:end, ])
  print(reg$coefficients[2])
  betas <- c(betas, c(reg$coefficients[2]))
  print(c(confint(reg, 'TR', level=0.95)))
  confints[i, ] <- c(confint(reg, 'TR', level=0.95))
}
ymin = min(c(betas, confints))
ymax = max(c(betas, confints))
plot(betas, ylim = c(ymin, ymax))
lines(confints[, 1], ylim = c(ymin, ymax))
lines(confints[, 2], ylim = c(ymin, ymax))
mean(betas)
plot(df$PER, df$TR)
