# Load necessary packages
library(tidyverse)
library(glmnet)
library(ggplot2)

# Set working directory (adjust the path as needed)
setwd("C:/Users/jsjch/OneDrive/Documents/github/sustainability-energy-matrix/energy_data")

# Load the datasets
oilEnergyUse <- read_csv("Energy use (kg of oil equivalent per capita).csv")
renewableEnergy <- read_csv("Renewable energy consumption (% of total final energy consumption).csv")
agriEmissions <- read_csv("Emissions_Agricultural_Energy use.csv")

# Data Preparation
# Identify year columns for filtering
year_columns_oilEnergyUse <- colnames(oilEnergyUse)[grepl("^\\d{4}$", colnames(oilEnergyUse))]
year_columns_renewableEnergy <- colnames(renewableEnergy)[grepl("^\\d{4}$", colnames(renewableEnergy))]

# Filter columns to include only 1990 to 2014
year_columns_oilEnergyUse <- year_columns_oilEnergyUse[year_columns_oilEnergyUse >= "1990" & year_columns_oilEnergyUse <= "2014"]
year_columns_renewableEnergy <- year_columns_renewableEnergy[year_columns_renewableEnergy >= "1990" & year_columns_renewableEnergy <= "2014"]

# Reshape datasets to long format using the filtered columns
oilEnergyUse_long <- oilEnergyUse %>%
  pivot_longer(cols = all_of(year_columns_oilEnergyUse),
               names_to = "Year",
               values_to = "Oil_Use",
               values_drop_na = TRUE)

renewableEnergy_long <- renewableEnergy %>%
  pivot_longer(cols = all_of(year_columns_renewableEnergy),
               names_to = "Year",
               values_to = "Renewable_Use",
               values_drop_na = TRUE)

# Convert Year column to numeric
oilEnergyUse_long$Year <- as.numeric(oilEnergyUse_long$Year)
renewableEnergy_long$Year <- as.numeric(renewableEnergy_long$Year)

# Filter agriEmissions for relevant Element (e.g., "CH4 emissions")
relevant_element <- "Emissions (CH4)"  # Adjust this to the relevant element in your dataset
agriEmissions_filtered <- agriEmissions %>%
  filter(Element == relevant_element)

# Ensure agriEmissions_filtered has Year and Value columns
if (!all(c("Year", "Value") %in% colnames(agriEmissions_filtered))) {
  stop("agriEmissions dataset must contain 'Year' and 'Value' columns.")
}

# Rename Value column to Emissions for consistency
agriEmissions_filtered <- agriEmissions_filtered %>%
  rename(Emissions = Value)

# Convert Year column to numeric in agriEmissions_filtered
agriEmissions_filtered$Year <- as.numeric(agriEmissions_filtered$Year)

# Aggregate agriEmissions_filtered dataset by Year
agriEmissions_agg <- agriEmissions_filtered %>%
  group_by(Year) %>%
  summarize(Emissions = mean(Emissions, na.rm = TRUE))

# Aggregate oilEnergyUse and renewableEnergy datasets by Year
oilEnergyUse_agg <- oilEnergyUse_long %>%
  group_by(Year) %>%
  summarize(Oil_Use = mean(Oil_Use, na.rm = TRUE))

renewableEnergy_agg <- renewableEnergy_long %>%
  group_by(Year) %>%
  summarize(Renewable_Use = mean(Renewable_Use, na.rm = TRUE))

# Merge aggregated datasets based on Year
combined_data <- reduce(list(oilEnergyUse_agg, renewableEnergy_agg, agriEmissions_agg), merge, by = "Year")

# Check for missing values and handle them
combined_data <- na.omit(combined_data)

# Identify and remove rows with zero or negative Renewable_Use
combined_data <- combined_data[combined_data$Renewable_Use > 0, ]

# Log transformation of variables
combined_data$log_Oil_Use <- log(combined_data$Oil_Use)
combined_data$log_Renewable_Use <- log(combined_data$Renewable_Use)
combined_data$log_Agri_Emissions <- log(combined_data$Emissions)

# Create a highRenewable variable (dummy) based on a threshold
combined_data$highRenewable <- ifelse(combined_data$Renewable_Use > median(combined_data$Renewable_Use, na.rm = TRUE), 1, 0)

# Ensure variation in the response variable
if (length(unique(combined_data$log_Renewable_Use)) <= 1) {
  stop("The response variable log_Renewable_Use has no variation. Please check the data.")
}

# Prepare data for regression
X <- model.matrix(log_Renewable_Use ~ log_Oil_Use + log_Agri_Emissions + highRenewable, data = combined_data)[, -1]
y <- combined_data$log_Renewable_Use

# Check the structure of the training data
str(X)
str(y)

# Standardize the predictors
X_standardized <- scale(X)

# Impute missing values in X_standardized with column means
X_standardized[is.na(X_standardized)] <- apply(X_standardized, 2, function(col) mean(col, na.rm = TRUE))[col(X_standardized)[is.na(X_standardized)]]

# Fit the Elastic Net model
# Define the file path to save the model
model_file <- "elastic_net_model.rds"

# Check if the model file exists
if (file.exists(model_file)) {
  # Load the saved model
  elastic_net_model <- readRDS(model_file)
} else {
  # Perform cross-validation and save the model
  elastic_net_model <- cv.glmnet(X_standardized, y, alpha = 0.5)
  saveRDS(elastic_net_model, model_file)
}

# Fit the final model using the best lambda from cross-validation
elastic_net_final <- glmnet(X_standardized, y, alpha = 0.5, lambda = elastic_net_model$lambda.min)

# Function to predict renewable energy use
predict_renewable_use <- function(log_Oil_Use, log_Agri_Emissions, highRenewable) {
  # Create a new data frame with the input values
  new_data <- data.frame(log_Oil_Use = log_Oil_Use, log_Agri_Emissions = log_Agri_Emissions, highRenewable = highRenewable)
  print("New data frame:")
  print(new_data)
  
  # Convert the new data frame to a matrix
  new_data_matrix <- as.matrix(new_data)
  cat("New data matrix columns:\n", colnames(new_data_matrix), "\n")
  cat("New data matrix dimensions:\n", dim(new_data_matrix), "\n")
  
  # Ensure the training data matrix X is defined
  if (!exists("X")) {
    stop("Training data matrix X is not defined.")
  }
  
  cat("Training data matrix columns:\n", colnames(X), "\n")
  cat("Training data matrix dimensions:\n", dim(X), "\n")
  
  # Ensure the new data matrix has the same number of columns as the training data
  if (ncol(new_data_matrix) != ncol(X)) {
    stop("New data matrix has a different number of columns than the training data.")
  }
  
  # Standardize the new data matrix using the training data scaling parameters
  new_data_matrix_standardized <- scale(new_data_matrix, center = colMeans(X), scale = apply(X, 2, sd))
  
  # Impute missing values in new_data_matrix_standardized with column means
  new_data_matrix_standardized[is.na(new_data_matrix_standardized)] <- apply(new_data_matrix_standardized, 2, function(col) mean(col, na.rm = TRUE))[col(new_data_matrix_standardized)[is.na(new_data_matrix_standardized)]]
  
  # Predict using the Elastic Net model
  predicted_value <- predict(elastic_net_final, new_data_matrix_standardized)
  return(predicted_value)
}

# Example usage of the function
log_Oil_Use_example <- log(5000)
log_Agri_Emissions_example <- log(1000)
highRenewable_example <- 1
predicted_value_example <- predict_renewable_use(log_Oil_Use_example, log_Agri_Emissions_example, highRenewable_example)
cat("Predicted Renewable Energy Use:", predicted_value_example, "\n")

# Generate predictions for a range of input values
log_Oil_Use_values <- seq(log(1000), log(10000), length.out = 100)
log_Agri_Emissions_values <- rep(log(1000), 100)
highRenewable_values <- rep(1, 100)
predictions <- mapply(predict_renewable_use, log_Oil_Use_values, log_Agri_Emissions_values, highRenewable_values)

# Create a dataframe for visualization
prediction_df <- data.frame(log_Oil_Use = log_Oil_Use_values, Predicted_Renewable_Use = as.numeric(predictions))

# Enhanced visualization
ggplot(prediction_df, aes(x = log_Oil_Use, y = Predicted_Renewable_Use)) +
  geom_line(aes(color = log_Oil_Use), size = 1) +
  geom_point(aes(color = log_Oil_Use), size = 2) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Predicted Renewable Energy Use vs. Log Oil Use",
       x = "Log Oil Use",
       y = "Predicted Renewable Energy Use",
       color = "Log Oil Use") +
  theme_minimal() +
  theme(legend.position = "right")