# Load libraries
install.packages("renv")
library(renv)

renv::restore() # If needing to install appropriate libraries

library(readr)
library(dplyr)
library(neuralnet)

# Used to check the renv lock file is up to date before commits
renv::status()

set.seed(123)
dataset = read_csv("all_data.csv")

# Remove the first 3 columns from the dataset
dataset <- dataset[, -c(1:3)]

# Convert the data to numerical
dataset <- data.frame(lapply(dataset, as.numeric))

# Scale the data for the ANN
for (i in names(dataset)[1:51]) {  
  dataset[[i]] <- (dataset[[i]] - min(dataset[[i]], na.rm = TRUE)) / 
    (max(dataset[[i]], na.rm = TRUE) - min(dataset[[i]], na.rm = TRUE))
}

# Instantiate dataframes
training_data <- dataset[1, ]
test_data <- dataset[1, ]

# Stratified splitting of data
train_start <- 2
train_end <- 5
start <- 1
end <- 4
for (i in 1:48) {
  training_data[train_start:train_end, ] <- dataset[start:end, ]
  test_data[i + 1, ] <- dataset[end + 1, ]
  train_start <- train_start + 4
  train_end <- train_end + 4
  start <- start + 5
  end <- end + 5
}

# Create a dataframe to store results
results_df <- data.frame(
  Target_Column = integer(),
  Num_Layers = integer(),
  Nodes = integer(),
  Stepmax = numeric(),
  Avg_Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# Determine target column indices
target_cols <- 52:54

# Initiate learning rate values and stepmax
learningrate_values <- c(10e-6, 10e-5, 10e-4, 10e-3, 10e-2)
Stepmax = 1e6

# Loop through the number of layers (1, 2, and 3)
for (num_layers in 1:3) {
  # Loop through the number of nodes (20 to 200, stepping by 20)
  for (nodes in seq(20, 200, by = 20)) {
    # Initialize a list to store accuracies for each target column
    target_accuracies <- vector("list", length(target_cols))
    
    # Initialize accuracy lists for each target
    for (i in seq_along(target_cols)) {
      target_accuracies[[i]] <- numeric()  # Ensure it's a numeric vector
    }
    
    # Loop through different stepmax values
    for (learningrate in learningrate_values) {
      # Repeat the training three times
      for (rep in 1:3) {
        # Loop through target variables
        for (i in seq_along(target_cols)) {
          target_col <- target_cols[i]
          
          # Extract target variables for train and test datasets
          traintarget <- training_data[[target_col]]
          traintarget <- as.factor(traintarget)
          testtarget <- test_data[[target_col]]
          testtarget <- as.factor(testtarget)
          
          # Create training and test datasets
          train <- training_data[, 1:50]
          names(train) <- make.names(names(train))
          test <- test_data[, 1:50]
          names(test) <- make.names(names(test))
          
          # Create hidden layer structure
          hidden_layers <- rep(nodes, num_layers)
          
          # Train the ANN
          ANN <- neuralnet(traintarget ~ ., data = train, hidden = hidden_layers, 
                           err.fct = 'ce', linear.output = FALSE, stepmax = Stepmax, rep=3, learningrate = learningrate)
          
          # Make predictions on the test set
          predictions <- predict(ANN, test)
          
          # Convert predictions to classes (adjust threshold as necessary)
          predicted_classes <- apply(predictions, 1, function(x) which.max(x))
          predicted_classes <- as.factor(predicted_classes)
          
          # Create confusion matrix
          confusion_matrix <- table(predicted_classes, testtarget)
          
          # Calculate accuracy
          accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
          
          # Store accuracy for the current target
          target_accuracies[[i]] <- c(target_accuracies[[i]], accuracy)
          
          # Print current accuracy
          cat("Layer:", num_layers, "Nodes:", nodes, "Stepmax:", Stepmax, "Learning Rate:", learningrate,
              "Target Column:", target_col, "Rep:", rep, 
              "Accuracy:", round(accuracy, 4), "\n")
        }
      }
      
      # Record average accuracy for each target column
      for (i in seq_along(target_cols)) {
        target_col <- target_cols[i]
        avg_accuracy <- mean(target_accuracies[[i]], na.rm = TRUE)  # Handle any NA values
        
        # Append the results to the dataframe
        results_df <- rbind(results_df, data.frame(
          Target_Column = target_col,
          Num_Layers = num_layers,
          Nodes = nodes,
          Stepmax = Stepmax,
          Learningrate = learningrate,
          Avg_Accuracy = round(avg_accuracy, 2),
          stringsAsFactors = FALSE
        ))
        
        # Print average accuracy
        cat("Average Accuracy for Target Column:", target_col, 
            "Num Layers:", num_layers, "Nodes:", nodes, 
            "Stepmax:", Stepmax, "Learning Rate:", learningrate, "Avg Accuracy:", round(avg_accuracy, 4), "\n")
      }
    }
  }
}

# Export the results to a CSV file
write.csv(results_df, "ann_accuracy_results_2.csv", row.names = TRUE)

# Print completion message
print("Results exported to ann_accuracy_results.csv")
