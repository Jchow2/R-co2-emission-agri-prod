# Load necessary packages
library(shiny)
library(ggplot2)
library(glmnet)

# Define the predict_renewable_use function
predict_renewable_use <- function(log_Oil_Use, log_Agri_Emissions, highRenewable) {
  # Create a new data frame with the input values
  new_data <- data.frame(log_Oil_Use = log_Oil_Use, log_Agri_Emissions = log_Agri_Emissions, highRenewable = highRenewable)
  
  # Convert the new data frame to a matrix
  new_data_matrix <- as.matrix(new_data)
  
  # Ensure the training data matrix X is defined
  if (!exists("X")) {
    stop("Training data matrix X is not defined.")
  }
  
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

# Define UI for the application
ui <- fluidPage(
  titlePanel("Elastic Regression Results: Agricultural Emissions and Energy Use"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("log_Oil_Use", "Log Oil Use:",
                  min = round(log(1000), 2), max = round(log(10000), 2),
                  value = round(log(5000), 2), step = 0.01),
      checkboxInput("highRenewable", "High Renewable Use", TRUE)
    ),
    
    mainPanel(
      plotOutput("predictionPlot"),
      verbatimTextOutput("predictedValue")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$predictionPlot <- renderPlot({
    log_Oil_Use_values <- seq(round(log(1000), 2), round(log(10000), 2), length.out = 100)
    log_Agri_Emissions_values <- rep(log(1000), 100)  # Hard-coded value for testing
    highRenewable_values <- rep(1, 100)  # Hard-coded value for testing
    
    predictions <- mapply(predict_renewable_use, log_Oil_Use_values, log_Agri_Emissions_values, highRenewable_values)
    
    prediction_df <- data.frame(log_Oil_Use = log_Oil_Use_values, Predicted_Renewable_Use = as.numeric(predictions))
    
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
  })
  
  output$predictedValue <- renderPrint({
    predicted_value <- predict_renewable_use(round(input$log_Oil_Use, 2), log(1000), 1)  # Hard-coded values for testing
    cat("Predicted Renewable Energy Use:", round(predicted_value, 2), "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

