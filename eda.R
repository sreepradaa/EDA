# Load the dataset and convert to data frame
data(Seatbelts)
seatbelts_df <- as.data.frame(Seatbelts)

# View the structure of the dataset
str(seatbelts_df)
# Check for missing values
sum(is.na(seatbelts_df))

# Handle missing values (if any)
seatbelts_df <- na.omit(seatbelts_df)
# Identify outliers using boxplot for 'DriversKilled'
boxplot(seatbelts_df$DriversKilled, main = "Boxplot for Drivers Killed", col = "orange")

# Calculate IQR for 'DriversKilled'
iqr <- IQR(seatbelts_df$DriversKilled)
lower_bound <- quantile(seatbelts_df$DriversKilled, 0.25) - 1.5 * iqr
upper_bound <- quantile(seatbelts_df$DriversKilled, 0.75) + 1.5 * iqr

# Filter out extreme outliers if needed
seatbelts_df <- subset(seatbelts_df, DriversKilled >= lower_bound & DriversKilled <= upper_bound)
# Check for duplicate rows
sum(duplicated(seatbelts_df))

# Remove duplicates (if any)
seatbelts_df <- seatbelts_df[!duplicated(seatbelts_df), ]
# Calculate mean, median, and mode for `DriversKilled`
mean_killed <- mean(seatbelts_df$DriversKilled)
median_killed <- median(seatbelts_df$DriversKilled)
mode_killed <- as.numeric(names(sort(table(seatbelts_df$DriversKilled), decreasing = TRUE)[1]))

cat("Mean:", mean_killed, "\nMedian:", median_killed, "\nMode:", mode_killed)
# Calculate standard deviation and variance for `DriversKilled`
sd_killed <- sd(seatbelts_df$DriversKilled)
var_killed <- var(seatbelts_df$DriversKilled)

cat("Standard Deviation:", sd_killed, "\nVariance:", var_killed)
# Plot histogram
hist(seatbelts_df$DriversKilled, main = "Distribution of Driver Fatalities", xlab = "Drivers Killed", col = "lightblue", border = "black")
# Boxplot for 'DriversKilled' before and after seatbelt law
boxplot(DriversKilled ~ law, data = seatbelts_df, main = "Drivers Killed Before and After Seatbelt Law", xlab = "Seatbelt Law (0=No, 1=Yes)", ylab = "Drivers Killed", col = c("red", "green"))
# Scatterplot
plot(seatbelts_df$PetrolPrice, seatbelts_df$DriversKilled, main = "Petrol Price vs. Driver Fatalities", xlab = "Petrol Price", ylab = "Drivers Killed", col = "blue", pch = 16)
# Time series plot
plot.ts(seatbelts_df$DriversKilled, main = "Monthly Driver Fatalities", ylab = "Drivers Killed", xlab = "Time", col = "darkgreen")
