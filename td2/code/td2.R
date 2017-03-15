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

df2 <- read.csv("~/Documents/1_ECP/3A/12_Time_Series/sch-master/td2/data_td2.csv", sep = ";")

# Question 1
# Rendement sans dividende
rendement <- function(data=df2, titre){
    titre <-  df2[[titre]]
    r <-  (titre[2:length(titre)]-titre[1:(length(titre)-1)]) / titre[1:(length(titre)-1)]
    return(ts(data = r, start = c(2005,12,30), frequency = 240))
}

par(mfrow=c(1,2))
rTOTAL <- rendement(titre='TOTAL')
ts.plot(rTOTAL)
rCAC40 <- rendement(titre='CAC40')
ts.plot(rCAC40) 
hist(rTOTAL, probability = TRUE, nclass = 30, main = "Densite empirique du rendement du titre TOTAL")
hist(rCAC40, probability = TRUE, nclass = 30, main = "Densite empirique du rendement de l'indice CAC40")

qqnorm(rTOTAL, main = "QQ plot du rendement de TOTAL")
qqline(rTOTAL)
qqnorm(rCAC40, main = "QQ plot du rendement du CAC40")
qqline(rCAC40)

# Question 2
r10Y = ts(df2[-nrow(df2), 'X10y']/100, start = c(2005,12,30), frequency = 240)

exces_titre <- rTOTAL - r10Y
exces_marche <- rCAC40 - r10Y
plot(exces_titre, exces_marche, main = "Rendement du titre TOTAL et du marche",
     xlab = "Rendement marche", ylab = "Rendement TOTAL")

# Question 4
lmodel <- lm(exces_titre~exces_marche)
summary(lmodel)

# Question 5
residus <- lmodel$residuals
plot_corr(lmodel$residuals, "Residus")
for (i in 1:15) print(Box.test(lmodel$residuals,lag=i,type='Ljung-Box')$p.value)


# Question 6

# Identification du mod??le ARMA adapt??
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
arima_fit = arima(rTOTAL, c(best_p, 0, best_q))

# Comparaison des RMSE la r??gression lin??aire et de la pr??diction ARMA
se_arima <- 0
se_lm <- 0
n <- 0
h <- 1
for (t in 2:(nrow(df2)-h-1)){
    
    # Valeur ?? pr??dire (en t+h)
    real <- exces_titre[t+h]
    
    # Pr??diction en t+h bas??e sur le pass?? [1:t]
    arima_fit <- arima(rTOTAL[1:t])
    pred1  <- predict(arima_fit, n.ahead = h, se.fit = F)[h]
    
    data = data.frame(y=exces_titre[1:t],x=exces_marche[1:t])
    lm <- lm(y~x, data)
    pred2 <- predict(lm, newdata = data.frame(x=exces_marche[t+h])) + r10Y[t+h]
    
    se_arima <- se_arima + (real-pred1)^2
    se_lm <- se_lm + (real-pred2)^2
    n <- n+1
}
rmse_arima <- sqrt(se_arima/n)
rmse_lm <- sqrt(se_lm/n)


# Test de Diebold et Mariano
dmTest <- function(h){
    # Differences d'erreurs de prediction
    d <- c()
    for (t in 2:(nrow(df2)-h)){
        
        # Valeur ?? pr??dire (en t+h)
        real <- exces_titre[t+h]
        
        # Pr??diction en t+h bas??e sur le pass?? [1:t]
        arima_fit <- arima(rTOTAL[1:t])
        pred1 <- predict(arima_fit, n.ahead = h, se.fit = F)[h]
        
        data = data.frame(y=exces_titre[1:t],x=exces_marche[1:t])
        lm <- lm(y~x, data)
        pred2 <- predict(lm, newdata = data.frame(x=exces_marche[t+h])) + r10Y[t+h]
        
        d[t-1] <-  (real-pred1)^2 - (real-pred2)^2
    }
    d <- d[1:length(d)-1]
    dbar <-  mean(d, na.rm=T)
    
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
dmTest(1)
