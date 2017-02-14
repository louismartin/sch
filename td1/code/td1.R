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
