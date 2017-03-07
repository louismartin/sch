plot_series <- function (x, name="Serie"){
  par(mfrow=c(3,1))
  plot(x, type='o', main=name)
  abline(h=0)
  acf(x, main="ACF")
  pacf(x, main="PACF")
}

# MA1
theta1 <- 0.8
MA1 <-arima.sim(n=200, list(ma=c(theta1)))
plot_series(MA1, "Serie MA")

# AR1
phi1 <- 0.8
AR1<-arima.sim(n=200, list(ar=c(phi1)))
plot_series(AR1, "Serie AR")

# Random walk
RW <- cumsum(rnorm(n=1000, sd=1))
plot_series(RW, "Marche aleatoire")


# Partie B
# Question 2
df = read.csv("cpi_2017.csv", header=T, sep = ";")
I = df$Etats.Unis
inflation = diff(log(I))
plot_series(inflation, name="Inflation")

# Question 3
plot_series(ts(inflation, frequency=12))
