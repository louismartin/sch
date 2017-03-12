plot_corr <- function (x, name="Serie"){
  par(mfrow=c(1,2))
  # plot(x, type='o', main=name)
  abline(h=0)
  acf(x, main=sprintf("%s ACF", name))
  pacf(x, main=sprintf("%s PACF", name))
}

# AR1
phi1 <- 0.5
AR1<-arima.sim(n=1000, list(ar=c(phi1)))
plot_corr(AR1, "Serie AR1")

# MA1
theta1 <- 0.5
MA1 <-arima.sim(n=1000, list(ma=c(theta1)))
plot_corr(MA1, "Serie MA1")

# Random walk
RW <- cumsum(rnorm(n=1000, sd=1))
par(mfrow=c(1,1))
plot(RW, type='l', main="Marche aleatoire")
plot_corr(RW, "Marche aleatoire")


# Partie B
df = read.csv("cpi_2017.csv", header=T, sep = ";")

# Question 2
indice_prix = ts(df$Etats.Unis, frequency=12, start=c(1996, 01))
inflation_mensuelle = diff(log(indice_prix))
plot_corr(inflation_mensuelle, name="Inflation mensuelle Etats-Unis")

# Question 3
inflation_annuelle = diff(log(indice_prix), lag=12)
par(mfrow=c(1,1))
plot(inflation_annuelle, type='l', main="Inflation annuelle Etats-Unis")
plot_corr(inflation_annuelle, "Inflation annuelle Etats-Unis")

# Question 4
indice_prix = ts(df$France, frequency=12, start=c(1996, 01))
par(mfrow=c(1,1))
plot(inflation_annuelle, type='l', main="Inflation annuelle France")
inflation_annuelle = diff(log(indice_prix), lag=12)
plot_corr(inflation_annuelle, "Inflation annuelle France")

# Question 5
best_aic <- Inf
best_p <- NULL
best_q <- NULL
for (p in 0:6){
  print(p)
  for (q in 0:13){
    aic <- arima(inflation_annuelle, order=c(p,0,q))$aic
    best_aic = min(aic, best_aic)
    if (aic == best_aic){
      print("best p ")
      print(p)
      print("best q ")
      print(q)
      best_p = p
      best_q = q
    }
  }
}

# Question 6
model = arima(inflation_annuelle, order=c(best_p,0,best_q), method="ML")

# Question 7
# Les residus sont deja calcules dans le model
plot_corr(model$residuals, "Residus")
# Previsions
for (months in c(3, 6, 12)) {
  pred = predict(model, n.ahead=months, level=5)
  upper_bound = pred$pred + 1.96*pred$se
  lower_bound = pred$pred - 1.96*pred$se
  y_max = max(upper_bound)
  y_min = min(lower_bound)

  par(mfrow=c(1,1))
  plot(pred$pred, ylim=c(y_min, y_max), main=sprintf("Predictions ?? %s mois", months) )
  lines(lower_bound, col="red")
  lines(upper_bound, col="red")
}

# Question 8
library(car)
# Durbin-Watson
test <- durbinWatsonTest(as.numeric(model$residuals))
print(test)

# Ljung-Box
for (i in 1:12) print(Box.test(model$residuals,lag=i,type='Ljung-Box')$p.value)


#############################
# Partie 2
#############################

df2 <- read_delim("~/Documents/1_ECP/3A/12_Time_Series/sch-master/td2/data_td2.csv", ";", 
                escape_double = FALSE, trim_ws = TRUE)

# Question 1
# Rendement sans dividende
rendement <- function(data=df2, titre){
    titre <-  df2[[titre]]
    return (titre[2:length(titre)]-titre[1:(length(titre)-1)]) / titre[1:(length(titre)-1)]
}
rTOTAL <- rendement(titre='TOTAL')
rCAC40 <- rendement(titre='CAC40')
 
hist(rTOTAL, probability = TRUE, nclass = 30, main = "Densite empirique du rendement du titre TOTAL")
hist(rCAC40, probability = TRUE, nclass = 30, main = "Densite empirique du rendement de l'indice CAC40")

qqnorm(rTOTAL)
qqline(rTOTAL)
qqnorm(rCAC40)
qqline(rCAC40)

# Question 2
r10Y <- rendement(titre='10y')

exces_titre <- rTOTAL - r10Y
exces_marche <- rCAC40 - r10Y
plot(exces_titre, exces_marche)

# Question 4
lmodel <- lm(exces_titre~exces_marche)
summary(lmodel)

# Question 5
residus <- lmodel$residuals
plot_corr(lmodel$residuals, "Residus")

# Question 6
plot_corr(rTOTAL)
arima(rTOTAL)
best_aic = Inf
for (p in 0:6){
    for (q in 0:15){
        aic <- arima(rTOTAL, order=c(p,0,q))$aic
        best_aic = min(aic, best_aic)
        if (aic == best_aic){
            print("best p ")
            print(p)
            print("best q ")
            print(q)
            best_p = p
            best_q = q
        }
    }
}
