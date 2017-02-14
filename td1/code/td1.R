filepath <- "../data/Data_centrale_fed_model_vf.csv"
df <- read.csv(filepath, sep=";")

# Remove empty rows
df[df==""] <- NA
df <- na.omit(df)

# Plot TER
plot(seq(1:length(df$PER)), df$PER)
