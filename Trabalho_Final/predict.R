# Install and load necessary libraries
install.packages("tidyverse")
install.packages("caret")
library(tidyverse)
library(caret)

dataset_path <- "C:/Users/Rafael/Desktop/TrabProdutos/Data/cereal.csv"

# Function to train, predict, evaluate, and update linear regression model
train_and_predict <- function(dataset_path, update_model = FALSE, new_observation = NULL) {
  
  # Read the dataset
  cereal_data <- read.csv(dataset_path)
  
  # Check if a new observation is provided
  if (!is.null(new_observation)) {
    # Add the new observation to the dataset
    cereal_data <- rbind(cereal_data, new_observation)
  }
  
  if ("name" %in% colnames(cereal_data)) {
    cereal_data <- cereal_data[, !colnames(cereal_data) %in% c("name")]
  }
  
  # Split the data into training and testing sets
  set.seed(123)  # for reproducibility
  split_index <- createDataPartition(cereal_data$rating, p = 0.8, list = FALSE)
  train_data <- cereal_data[split_index, ]
  test_data <- cereal_data[-split_index, ]
  
  # Build a linear regression model
  lm_model <- lm(rating ~ ., data = train_data)
  
  # Make predictions on the test set
  predictions <- predict(lm_model, newdata = test_data)
  
  # Evaluate the model
  mse <- mean((test_data$rating - predictions)^2)
  rmse <- sqrt(mse)
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  
  # Print the coefficients to see their impact on the rating
  cat("Coefficients:\n")
  print(coef(lm_model))
  
  # Save the model for future use
  saveRDS(lm_model, "linear_model.rds")  # Change the file name as needed
  
  if (update_model) {
    cat("Model updated with new observation.\n")
  }
}

# Example usage:
# Replace "your_dataset.csv" with the actual file path
# You can add new observations by providing the 'new_observation' argument
# 
train_and_predict(dataset_path, update_model = TRUE)

# Function to predict rating based on a given model and new observation
predict_rating <- function(model, new_observation) {
  prediction <- predict(model, newdata = new_observation)
  return(prediction)
}

# Example usage:
# Load the saved model
saved_model <- readRDS("linear_model.rds")  # Change the file name if needed

# Create a new observation (replace with actual values)
new_observation <- data.frame(
  name = "NewBrand",
  mfr = "N",
  type = "C",
  calories = 100,
  protein = 3,
  fat = 1,
  sodium = 200,
  fiber = 5,
  carbo = 15,
  sugars = 5,
  potass = 200,
  vitamins = 25,
  shelf = 3,
  weight = 1,
  cups = 0.8
)

# Predict the rating using the saved model and new observation
predicted_rating <- predict_rating(saved_model, new_observation)
cat("Predicted Rating:", predicted_rating, "\n")
