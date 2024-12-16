# Clear directory
rm(list = ls())

# Set working directory
setwd("C:/Users/jsjch/OneDrive/Documents/github/sustainability-energy-matrix/energy_data")

# Load necessary libraries
library(AER)
library(dplyr)
library(doBy)
library(ggplot2)
library(stargazer)
library(tidyr)
library(erer)

# Load datasets
agriEmissions <- read.csv("Emissions_Agricultural_Energy use.csv", header = TRUE, sep = ",")
oilEnergyUse <- read.csv("Energy use (kg of oil equivalent per capita).csv", header = TRUE, sep = ",")
renewableEnergy <- read.csv("Renewable energy consumption (% of total final energy consumption).csv", header = TRUE, sep = ",")

# Function to clean column names
clean_column_names <- function(df) {
  colnames(df) <- gsub("^X", "", colnames(df))
  return(df)
}

# Clean column names
agriEmissions <- clean_column_names(agriEmissions)
oilEnergyUse <- clean_column_names(oilEnergyUse)
renewableEnergy <- clean_column_names(renewableEnergy)

# Handle missing data
agriEmissions <- na.omit(agriEmissions)
oilEnergyUse <- na.omit(oilEnergyUse)
renewableEnergy <- na.omit(renewableEnergy)

# Check for empty data frames
cat("Number of rows in agriEmissions:", nrow(agriEmissions), "\n")
cat("Number of rows in oilEnergyUse:", nrow(oilEnergyUse), "\n")
cat("Number of rows in renewableEnergy:", nrow(renewableEnergy), "\n")

# Keep only necessary columns for stargazer
agriEmissions_subset <- agriEmissions[c("Value")]
oilEnergyUse_subset <- oilEnergyUse[c("2014")]
renewableEnergy_subset <- renewableEnergy[c("2022")]

# Ensure correct data types
agriEmissions_subset$Value <- as.numeric(agriEmissions_subset$Value)
oilEnergyUse_subset$`2014` <- as.numeric(oilEnergyUse_subset$`2014`)
renewableEnergy_subset$`2022` <- as.numeric(renewableEnergy_subset$`2022`)

# Descriptive Statistics
stargazer(agriEmissions_subset, type="text", digits=2, title="Descriptive Statistics for Agricultural Emissions")
stargazer(oilEnergyUse_subset, type="text", digits=2, title="Descriptive Statistics for Oil Energy Use in 2014")
stargazer(renewableEnergy_subset, type="text", digits=2, title="Descriptive Statistics for Renewable Energy Use in 2022")

# Aggregate emissions data by year
agg_emissions <- agriEmissions %>%
  group_by(Year) %>%
  summarise(Total_Emissions = sum(Value))

## Exploratory Data Analysis
# Line plot showing the overall trend of agricultural emissions over the years
plot1 <- ggplot(agg_emissions, aes(x = Year, y = Total_Emissions, fill = Total_Emissions)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total Agricultural Emissions Over Time", x = "Year", y = "Total Emissions (kt)", fill = "Emissions (kt)") +
  theme_minimal()

# Aggregate oil energy use data by year
# Convert data from wide to long format
oilEnergyUse_long <- oilEnergyUse %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "Year", values_to = "Energy_Use")

# Convert Year to numeric
oilEnergyUse_long$Year <- as.numeric(oilEnergyUse_long$Year)

# Aggregate data by year
agg_oil_use <- oilEnergyUse_long %>%
  group_by(Year) %>%
  summarise(Total_Energy_Use = sum(Energy_Use, na.rm = TRUE))

# Create bar chart with linear trendline
plot2 <- ggplot(agg_oil_use, aes(x = Year, y = Total_Energy_Use, fill = Total_Energy_Use)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total Oil Energy Use Over Time", x = "Year", y = "Total Energy Use (kg of oil equivalent per capita)", fill = "Energy Use") +
  theme_minimal()

# Histogram for renewable energy use in 2022 with color gradient
plot3 <- ggplot(renewableEnergy, aes(x = `2022`)) +
  geom_histogram(binwidth = 2, aes(fill = ..count..), color = "black", alpha = 0.7) +
  scale_fill_gradient("Count", low = "lightblue", high = "blue") +
  xlab("Renewable Energy Use (% of total final energy consumption)") +
  ylab("Frequency") +
  labs(title = "Renewable Energy Use in 2022") +
  theme_minimal()

# Create dummy variables based on thresholds
renewableEnergy$highRenewable <- ifelse(renewableEnergy$`2022` > 20, "High", "Low")

# Histogram plot with categories and annotations
plot4 <- ggplot(renewableEnergy, aes(x = `2022`, fill = highRenewable)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.6, color = "black") +
  scale_fill_manual(values = c("High" = "blue", "Low" = "lightblue")) +
  xlab("Renewable Energy Use (% of total final energy consumption)") +
  ylab("Frequency") +
  labs(title = "Distribution of Renewable Energy Use in 2022") +
  theme_minimal() +
  annotate("text", x = 25, y = 15, label = "Threshold: 20%", color = "red", size = 3)

# Print the plots
print(plot1)
print(plot2)
print(plot3)
print(plot4)

# Handle zeros before log transformation
agriEmissions$Value[agriEmissions$Value == 0] <- 0.01
oilEnergyUse$`2014`[oilEnergyUse$`2014` == 0] <- 0.01
renewableEnergy$`2022`[renewableEnergy$`2022` == 0] <- 0.01

# Transform and create dummy variables for log transformations and thresholds
# Log transformations
agriEmissions$log_Emissions <- log(agriEmissions$Value)
oilEnergyUse$log_Oil_Use <- log(oilEnergyUse$`2014`)
renewableEnergy$log_Renewable_Use <- log(renewableEnergy$`2022`)

# Remove NA/NaN/Inf values after transformation
agriEmissions <- agriEmissions[is.finite(agriEmissions$log_Emissions), ]
oilEnergyUse <- oilEnergyUse[is.finite(oilEnergyUse$log_Oil_Use), ]
renewableEnergy <- renewableEnergy[is.finite(renewableEnergy$log_Renewable_Use), ]

# Create dummy variables based on thresholds 
renewableEnergy$highRenewable <- ifelse(renewableEnergy$`2022` > 20, 1, 0)

# Run regression analysis using lm command 
regr1 <- lm(log_Emissions ~ Year, data = agriEmissions) 
regr2 <- lm(log_Oil_Use ~ Country.Name, data = oilEnergyUse) 
regr3 <- lm(log_Renewable_Use ~ Country.Name, data = renewableEnergy) 
regr4 <- lm(log_Renewable_Use ~ highRenewable, data = renewableEnergy) 

# Using stargazer to display regression results
stargazer(regr1, regr2, regr3, regr4,
          title = "Regression Analysis of Energy Use",
          type = "text", df = FALSE, digits = 3)

# Prediction using regression model with regr4 (highRenewable)
predictRenewableUse_regr4 <- data.frame(highRenewable = 1)
predicted_value_regr4 <- predict(regr4, newdata = predictRenewableUse_regr4)
cat("Predicted Renewable Energy Use for high renewable:", predicted_value_regr4, "\n")

# Summary and insights
summary(regr1)
summary(regr2)
summary(regr3)
summary(regr4)

# F-test on interaction term (example, replace with actual model and variables)
# lht(regr3, c("log_Oil_Use = highRenewable"), white.adjust = "hc1")

# Using the results of a regression model to predict an outcome
# Adjusted for current available data
predictRenewableUse <- data.frame(highRenewable = 1)
predicted_value <- predict(regr4, newdata = predictRenewableUse)
cat("Predicted Renewable Energy Use for high renewable:", predicted_value, "\n")

# Create dummy variable of a crop index for countries above the 50th percentile
renewableEnergy$medRenewable <- ifelse(renewableEnergy$log_Renewable_Use > median(renewableEnergy$log_Renewable_Use, na.rm = TRUE), 1, 0)

# Run probit- and logit- models using glm command
p1 <- glm(medRenewable ~ highRenewable, family = binomial(link = "probit"), x = TRUE, data = renewableEnergy)
l1 <- glm(medRenewable ~ highRenewable, family = binomial, x = TRUE, data = renewableEnergy)

# Using stargazer to display regression table
stargazer(p1, l1,
          se = list(NULL, NULL),
          title = "Probit- and Logit- Model of Renewable Energy Use",
          type = "text", df = FALSE, digits = 3,
          omit.stat = c("f"))

# Calculate marginal error from probit- and logit- regression
fm1 <- maBina(p1, x.mean = FALSE, rev.dum = TRUE, digits = 3)
fm2 <- maBina(l1, x.mean = FALSE, rev.dum = TRUE, digits = 3)

# Using stargazer to display the marginal effects of the probit and logit regressions
stargazer(p1, fm1, l1, fm2,
          se = list(NULL, NULL, NULL, NULL),
          title = "Marginal Effects",
          type = "text", star.cutoffs = NA, df = FALSE, digits = 3,
          keep.stat = c("n", "ll"))

# Calculate the pseudo-R2
pseudoR2p1 <- (p1$null.deviance - p1$deviance) / p1$null.deviance
pseudoR2l1 <- (l1$null.deviance - l1$deviance) / l1$null.deviance
cat("Pseudo-R2 for Probit Model:", pseudoR2p1, "\n")
cat("Pseudo-R2 for Logit Model:", pseudoR2l1, "\n")

