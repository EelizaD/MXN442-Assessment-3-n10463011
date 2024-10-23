# Load libraries
library(readr)
library(dplyr)

install.packages("caTools")    # For Logistic regression 
library(caTools)

install.packages('randomForest') # For generating random forest model
library(randomForest)

install.packages('caret')                    # classification and regression training : The library caret has a function to make prediction.
library(caret)
install.packages('e1071', dependencies=TRUE)

set.seed(123)
dataset = read_csv("all_data.csv")

#Copy the column labels for the training and testing set
training_data <- dataset[1, ]
test_data <- dataset[1, ]

#Separate the data into training and testing. As per the paper, for each 5 rows
#of data, 4 rows go to training, and 1 row goes to testing
train_start <- 1
train_end <- 4
start <- 1
end <- 4
for (i in 1:48) {
  training_data[train_start:train_end, ] <- dataset[start:end, ]
  test_data[i+1, ] <- dataset[end+1, ]
  train_start <- train_start + 4
  train_end <- train_end + 4
  start <- start + 5
  end <- end + 5
}


# Create training and test datasets that only contain model factors
train <- training_data[, 4:53]
names(train) <- make.names(names(train))
test <- test_data[, 4:53]
names(test) <- make.names(names(test))


# Fit the RF3 model! ------------------------------------------------------

# Loop through each number of trees
# Store accuracy results
accuracy_results_RF3 <- numeric(10)  # For 10:100 in steps of 10

#Create a vector with the different numbers of trees to use
notrees <- seq(10, 100, by=10)

# Extract target variables for train and test datasets
traintarget_RF3 <- training_data[[56]]
traintarget_RF3 <- as.factor(traintarget_RF3)
testtarget_RF3 <- test_data[[56]]
testtarget_RF3 <- as.factor(testtarget_RF3)

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3 <- randomForest(traintarget_RF3 ~ ., data = train, ntree = notrees[i], 
                       proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3, newdata = test)
  confusion_matrix_RF3 <- table(testtarget_RF3, predictions)
  accuracy_RF3 <- sum(diag(confusion_matrix_RF3)) / sum(confusion_matrix_RF3)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3[i] <- accuracy_RF3  # Store the accuracy at the appropriate index
}

# Combine accuracy results with target names
results_df_RF3 <- data.frame(Trees = notrees, Accuracy = accuracy_results_RF3)

# Print the results
print(results_df_RF3)

# Print all accuracies
print("Accuracy results for each number of trees (RF3 model)")
print(accuracy_results_RF3)

print("Average accuracy for RF3 model")
mean(accuracy_results_RF3)

#Finally, plot the accuracy against the number of trees
plot(notrees, accuracy_results_RF3, pch=19, col="#9999FF", main="Accuracy of RF3
     model against the number of trees", xlab="Number of trees", 
     ylab="Accuracy of RF3 model", ylim=c(0.6, 1))

# Model reduction for RF3 model! ------------------------------------------

#The section of fitting the RF3 model will need to be run for this section to work

#Get the importance values
RF3_importance_all <- importance(rf_3)
print(RF3_importance_all)

#Convert to a dataframe to identify 5 minimum values
RF3_importance_all_df <- as.data.frame(RF3_importance_all)

#Add variable names
RF3_importance_all_df$Variable <- rownames(RF3_importance_all_df)

#Identify 5 minimum values
RF3_min_importance_all <- RF3_importance_all_df[order(RF3_importance_all_df$MeanDecreaseGini), ][1:5, ]

#Print the results
print(RF3_min_importance_all)

#Now remove the least important variables and refit RF model
train_2_exclude <- train[ ,RF3_min_importance_all[ ,2]]
columns_include <- setdiff(names(train), names(train_2_exclude))
train_2 <- train[ ,columns_include]
names(train_2) <- make.names(names(train_2))
test_2_exclude <- test[ ,RF3_min_importance_all[ ,2]]
columns_include <- setdiff(names(test), names(test_2_exclude))
test_2 <- test[ ,columns_include]
names(test_2) <- make.names(names(test_2))

#Now refit the model and test the accuracy
accuracy_results_RF3_2 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_2 <- randomForest(traintarget_RF3 ~ ., data = train_2, ntree = notrees[i], 
                         proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_2, newdata = test_2)
  confusion_matrix_RF3_2 <- table(testtarget_RF3, predictions)
  accuracy_RF3_2 <- sum(diag(confusion_matrix_RF3_2)) / sum(confusion_matrix_RF3_2)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_2[i] <- accuracy_RF3_2  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_2)

#The accuracy is 77.14% (previously 76.73%), so now find the next 5 least 
#important factors and remove them

#Identify least important factors
RF3_importance_2 <- importance(rf_3_2)
print(RF3_importance_2)

#Convert to a dataframe to identify 5 minimum values
RF3_importance_2_df <- as.data.frame(RF3_importance_2)

#Add variable names
RF3_importance_2_df$Variable <- rownames(RF3_importance_2_df)

#Identify 5 minimum values
RF3_min_importance_2 <- RF3_importance_2_df[order(RF3_importance_2_df$MeanDecreaseGini), ][1:5, ]

#Print the results
print(RF3_min_importance_2)

#Now remove the least important variables and refit RF model
train_3_exclude <- train_2[ ,RF3_min_importance_2[ ,2]]
columns_include <- setdiff(names(train_2), names(train_3_exclude))
train_3 <- train_2[ ,columns_include]
names(train_3) <- make.names(names(train_3))
test_3_exclude <- test_2[ ,RF3_min_importance_2[ ,2]]
columns_include <- setdiff(names(test_2), names(test_3_exclude))
test_3 <- test_2[ ,columns_include]
names(test_3) <- make.names(names(test_3))

#Now refit the model and test the accuracy
accuracy_results_RF3_3 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_3 <- randomForest(traintarget_RF3 ~ ., data = train_3, ntree = notrees[i], 
                         proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_3, newdata = test_3)
  confusion_matrix_RF3_3 <- table(testtarget_RF3, predictions)
  accuracy_RF3_3 <- sum(diag(confusion_matrix_RF3_3)) / sum(confusion_matrix_RF3_3)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_3[i] <- accuracy_RF3_3  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_3)

#The accuracy is at 75.51% (previously 77.14%), so now find the next 5 least 
#important factors and remove them

#Identify least important factors
RF3_importance_3 <- importance(rf_3_3)
print(RF3_importance_3)

#Convert to a dataframe to identify 5 minimum values
RF3_importance_3_df <- as.data.frame(RF3_importance_3)

#Add variable names
RF3_importance_3_df$Variable <- rownames(RF3_importance_3_df)

#Identify 5 minimum values
RF3_min_importance_3 <- RF3_importance_3_df[order(RF3_importance_3_df$MeanDecreaseGini), ][1:5, ]

#Print the results
print(RF3_min_importance_3)

#Now remove the least important variables and refit RF model
train_4_exclude <- train_3[ ,RF3_min_importance_3[ ,2]]
columns_include <- setdiff(names(train_3), names(train_4_exclude))
train_4 <- train_3[ ,columns_include]
names(train_4) <- make.names(names(train_4))
test_4_exclude <- test_3[ ,RF3_min_importance_3[ ,2]]
columns_include <- setdiff(names(test_3), names(test_4_exclude))
test_4 <- test_3[ ,columns_include]
names(test_4) <- make.names(names(test_4))

#Now refit the model and test the accuracy
accuracy_results_RF3_4 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_4 <- randomForest(traintarget_RF3 ~ ., data = train_4, ntree = notrees[i], 
                         proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_4, newdata = test_4)
  confusion_matrix_RF3_4 <- table(testtarget_RF3, predictions)
  accuracy_RF3_4 <- sum(diag(confusion_matrix_RF3_4)) / sum(confusion_matrix_RF3_4)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_4[i] <- accuracy_RF3_4  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_4)

#The accuracy is at 77.14% (previously 75.51%), so now find the next 5 least 
#important factors and remove them

#Identify least important factors
RF3_importance_4 <- importance(rf_3_4)
print(RF3_importance_4)

#Convert to a dataframe to identify 5 minimum values
RF3_importance_4_df <- as.data.frame(RF3_importance_4)

#Add variable names
RF3_importance_4_df$Variable <- rownames(RF3_importance_4_df)

#Identify 5 minimum values
RF3_min_importance_4 <- RF3_importance_4_df[order(RF3_importance_4_df$MeanDecreaseGini), ][1:5, ]

#Print the results
print(RF3_min_importance_4)

#Now remove the least important variables and refit RF model
train_5_exclude <- train_4[ ,RF3_min_importance_4[ ,2]]
columns_include <- setdiff(names(train_4), names(train_5_exclude))
train_5 <- train_4[ ,columns_include]
names(train_5) <- make.names(names(train_5))
test_5_exclude <- test_4[ ,RF3_min_importance_4[ ,2]]
columns_include <- setdiff(names(test_4), names(test_5_exclude))
test_5 <- test_4[ ,columns_include]
names(test_5) <- make.names(names(test_5))

#Now refit the model and test the accuracy
accuracy_results_RF3_5 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_5 <- randomForest(traintarget_RF3 ~ ., data = train_5, ntree = notrees[i], 
                         proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_5, newdata = test_5)
  confusion_matrix_RF3_5 <- table(testtarget_RF3, predictions)
  accuracy_RF3_5 <- sum(diag(confusion_matrix_RF3_5)) / sum(confusion_matrix_RF3_5)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_5[i] <- accuracy_RF3_5  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_5)

#The accuracy is at 76.53% (previously 77.14%), so now find the next 5 least 
#important factors and remove them

#Identify least important factors
RF3_importance_5 <- importance(rf_3_5)
print(RF3_importance_5)

#Convert to a dataframe to identify 5 minimum values
RF3_importance_5_df <- as.data.frame(RF3_importance_5)

#Add variable names
RF3_importance_5_df$Variable <- rownames(RF3_importance_5_df)

#Identify 5 minimum values
RF3_min_importance_5 <- RF3_importance_5_df[order(RF3_importance_5_df$MeanDecreaseGini), ][1:5, ]

#Print the results
print(RF3_min_importance_5)

#Now remove the least important variables and refit RF model
train_6_exclude <- train_5[ ,RF3_min_importance_5[ ,2]]
columns_include <- setdiff(names(train_5), names(train_6_exclude))
train_6 <- train_5[ ,columns_include]
names(train_6) <- make.names(names(train_6))
test_6_exclude <- test_5[ ,RF3_min_importance_5[ ,2]]
columns_include <- setdiff(names(test_5), names(test_6_exclude))
test_6 <- test_5[ ,columns_include]
names(test_6) <- make.names(names(test_6))

#Now refit the model and test the accuracy
accuracy_results_RF3_6 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_6 <- randomForest(traintarget_RF3 ~ ., data = train_6, ntree = notrees[i], 
                         proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_6, newdata = test_6)
  confusion_matrix_RF3_6 <- table(testtarget_RF3, predictions)
  accuracy_RF3_6 <- sum(diag(confusion_matrix_RF3_6)) / sum(confusion_matrix_RF3_6)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_6[i] <- accuracy_RF3_6  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_6)

#The accuracy is at 76.12% (previously 76.53%), so now find the next 5 least 
#important factors and remove them

#Identify least important factors
RF3_importance_6 <- importance(rf_3_6)
print(RF3_importance_6)

#Convert to a dataframe to identify 5 minimum values
RF3_importance_6_df <- as.data.frame(RF3_importance_6)

#Add variable names
RF3_importance_6_df$Variable <- rownames(RF3_importance_6_df)

#Identify 5 minimum values
RF3_min_importance_6 <- RF3_importance_6_df[order(RF3_importance_6_df$MeanDecreaseGini), ][1:5, ]

#Print the results
print(RF3_min_importance_6)

#Now remove the least important variables and refit RF model
train_7_exclude <- train_6[ ,RF3_min_importance_6[ ,2]]
columns_include <- setdiff(names(train_6), names(train_7_exclude))
train_7 <- train_6[ ,columns_include]
names(train_7) <- make.names(names(train_7))
test_7_exclude <- test_6[ ,RF3_min_importance_6[ ,2]]
columns_include <- setdiff(names(test_6), names(test_7_exclude))
test_7 <- test_6[ ,columns_include]
names(test_7) <- make.names(names(test_7))

#Now refit the model and test the accuracy
accuracy_results_RF3_7 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_7 <- randomForest(traintarget_RF3 ~ ., data = train_7, ntree = notrees[i], 
                         proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_7, newdata = test_7)
  confusion_matrix_RF3_7 <- table(testtarget_RF3, predictions)
  accuracy_RF3_7 <- sum(diag(confusion_matrix_RF3_7)) / sum(confusion_matrix_RF3_7)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_7[i] <- accuracy_RF3_7  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_7)

#The accuracy is at 76.12% (previously 76.12%), so now find the next 5 least 
#important factors and remove them

#Identify least important factors
RF3_importance_7 <- importance(rf_3_7)
print(RF3_importance_7)

#Convert to a dataframe to identify 5 minimum values
RF3_importance_7_df <- as.data.frame(RF3_importance_7)

#Add variable names
RF3_importance_7_df$Variable <- rownames(RF3_importance_7_df)

#Identify 5 minimum values
RF3_min_importance_7 <- RF3_importance_7_df[order(RF3_importance_7_df$MeanDecreaseGini), ][1:5, ]

#Print the results
print(RF3_min_importance_7)

#Now remove the least important variables and refit RF model
train_8_exclude <- train_7[ ,RF3_min_importance_7[ ,2]]
columns_include <- setdiff(names(train_7), names(train_8_exclude))
train_8 <- train_7[ ,columns_include]
names(train_8) <- make.names(names(train_8))
test_8_exclude <- test_7[ ,RF3_min_importance_7[ ,2]]
columns_include <- setdiff(names(test_7), names(test_8_exclude))
test_8 <- test_7[ ,columns_include]
names(test_8) <- make.names(names(test_8))

#Now refit the model and test the accuracy
accuracy_results_RF3_8 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_8 <- randomForest(traintarget_RF3 ~ ., data = train_8, ntree = notrees[i], 
                         proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_8, newdata = test_8)
  confusion_matrix_RF3_8 <- table(testtarget_RF3, predictions)
  accuracy_RF3_8 <- sum(diag(confusion_matrix_RF3_8)) / sum(confusion_matrix_RF3_8)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_8[i] <- accuracy_RF3_8  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_8)

#The accuracy is at 74.69% (previously 76.12%), so now find the next 5 least 
#important factors and remove them

#Identify least important factors
RF3_importance_7 <- importance(rf_3_7)
print(RF3_importance_7)

#Convert to a dataframe to identify 5 minimum values
RF3_importance_7_df <- as.data.frame(RF3_importance_7)

#Add variable names
RF3_importance_7_df$Variable <- rownames(RF3_importance_7_df)

#Identify 5 minimum values
RF3_min_importance_7 <- RF3_importance_7_df[order(RF3_importance_7_df$MeanDecreaseGini), ][1:5, ]

#Print the results
print(RF3_min_importance_7)

#Now remove the least important variables and refit RF model
train_8_exclude <- train_7[ ,RF3_min_importance_7[ ,2]]
columns_include <- setdiff(names(train_7), names(train_8_exclude))
train_8 <- train_7[ ,columns_include]
names(train_8) <- make.names(names(train_8))
test_8_exclude <- test_7[ ,RF3_min_importance_7[ ,2]]
columns_include <- setdiff(names(test_7), names(test_8_exclude))
test_8 <- test_7[ ,columns_include]
names(test_8) <- make.names(names(test_8))

#Now refit the model and test the accuracy
accuracy_results_RF3_8 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_8 <- randomForest(traintarget_RF3 ~ ., data = train_8, ntree = notrees[i], 
                         proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_8, newdata = test_8)
  confusion_matrix_RF3_8 <- table(testtarget_RF3, predictions)
  accuracy_RF3_8 <- sum(diag(confusion_matrix_RF3_8)) / sum(confusion_matrix_RF3_8)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_8[i] <- accuracy_RF3_8  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_8)

#The accuracy is at 76.33% (previously 74.69%), so now find the next 5 least 
#important factors and remove them

#Identify least important factors
RF3_importance_8 <- importance(rf_3_8)
print(RF3_importance_8)

#Convert to a dataframe to identify 5 minimum values
RF3_importance_8_df <- as.data.frame(RF3_importance_8)

#Add variable names
RF3_importance_8_df$Variable <- rownames(RF3_importance_8_df)

#Identify 5 minimum values
RF3_min_importance_8 <- RF3_importance_8_df[order(RF3_importance_8_df$MeanDecreaseGini), ][1:5, ]

#Print the results
print(RF3_min_importance_8)

#Now remove the least important variables and refit RF model
train_9_exclude <- train_8[ ,RF3_min_importance_8[ ,2]]
columns_include <- setdiff(names(train_8), names(train_9_exclude))
train_9 <- train_8[ ,columns_include]
names(train_9) <- make.names(names(train_9))
test_9_exclude <- test_8[ ,RF3_min_importance_8[ ,2]]
columns_include <- setdiff(names(test_8), names(test_9_exclude))
test_9 <- test_8[ ,columns_include]
names(test_9) <- make.names(names(test_9))

#Now refit the model and test the accuracy
accuracy_results_RF3_9 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_9 <- randomForest(traintarget_RF3 ~ ., data = train_9, ntree = notrees[i], 
                         proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_9, newdata = test_9)
  confusion_matrix_RF3_9 <- table(testtarget_RF3, predictions)
  accuracy_RF3_9 <- sum(diag(confusion_matrix_RF3_9)) / sum(confusion_matrix_RF3_9)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_9[i] <- accuracy_RF3_9  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_9)

#The accuracy is at 75.71% (previously 76.33%). Now that we're down to 10 variables, 
#only take out one at a time.

#Identify least important factors
RF3_importance_9 <- importance(rf_3_9)
print(RF3_importance_9)

#Convert to a dataframe to identify minimum value
RF3_importance_9_df <- as.data.frame(RF3_importance_9)

#Add variable names
RF3_importance_9_df$Variable <- rownames(RF3_importance_9_df)

#Identify minimum values
RF3_min_importance_9 <- RF3_importance_9_df[order(RF3_importance_9_df$MeanDecreaseGini), ][1, ]

#Print the results
print(RF3_min_importance_9)

#Now remove the least important variables and refit RF model
train_10_exclude <- train_9[ ,RF3_min_importance_9[ ,2]]
columns_include <- setdiff(names(train_9), names(train_10_exclude))
train_10 <- train_9[ ,columns_include]
names(train_10) <- make.names(names(train_10))
test_10_exclude <- test_9[ ,RF3_min_importance_9[ ,2]]
columns_include <- setdiff(names(test_9), names(test_10_exclude))
test_10 <- test_9[ ,columns_include]
names(test_10) <- make.names(names(test_10))

#Now refit the model and test the accuracy
accuracy_results_RF3_10 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_10 <- randomForest(traintarget_RF3 ~ ., data = train_10, ntree = notrees[i], 
                          proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_10, newdata = test_10)
  confusion_matrix_RF3_10 <- table(testtarget_RF3, predictions)
  accuracy_RF3_10 <- sum(diag(confusion_matrix_RF3_10)) / sum(confusion_matrix_RF3_10)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_10[i] <- accuracy_RF3_10  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_10)

#The accuracy is at 75.92% (previously 75.71%). Now that we're down to 10 variables, 
#only take out one at a time.

#Identify least important factors
RF3_importance_10 <- importance(rf_3_10)
print(RF3_importance_10)

#Convert to a dataframe to identify minimum value
RF3_importance_10_df <- as.data.frame(RF3_importance_10)

#Add variable names
RF3_importance_10_df$Variable <- rownames(RF3_importance_10_df)

#Identify minimum values
RF3_min_importance_10 <- RF3_importance_10_df[order(RF3_importance_10_df$MeanDecreaseGini), ][1, ]

#Print the results
print(RF3_min_importance_10)

#Now remove the least important variables and refit RF model
train_11_exclude <- train_10[ ,RF3_min_importance_10[ ,2]]
columns_include <- setdiff(names(train_10), names(train_11_exclude))
train_11 <- train_10[ ,columns_include]
names(train_11) <- make.names(names(train_11))
test_11_exclude <- test_10[ ,RF3_min_importance_10[ ,2]]
columns_include <- setdiff(names(test_10), names(test_11_exclude))
test_11 <- test_10[ ,columns_include]
names(test_11) <- make.names(names(test_11))

#Now refit the model and test the accuracy
accuracy_results_RF3_11 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_11 <- randomForest(traintarget_RF3 ~ ., data = train_11, ntree = notrees[i], 
                          proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_11, newdata = test_11)
  confusion_matrix_RF3_11 <- table(testtarget_RF3, predictions)
  accuracy_RF3_11 <- sum(diag(confusion_matrix_RF3_11)) / sum(confusion_matrix_RF3_11)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_11[i] <- accuracy_RF3_11  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_11)

#The accuracy is at 77.35% (previously 75.92%). Now that we're down to 10 variables, 
#only take out one at a time.

#Identify least important factor
RF3_importance_11 <- importance(rf_3_11)
print(RF3_importance_11)

#Convert to a dataframe to identify minimum value
RF3_importance_11_df <- as.data.frame(RF3_importance_11)

#Add variable names
RF3_importance_11_df$Variable <- rownames(RF3_importance_11_df)

#Identify minimum values
RF3_min_importance_11 <- RF3_importance_11_df[order(RF3_importance_11_df$MeanDecreaseGini), ][1, ]

#Print the results
print(RF3_min_importance_11)

#Now remove the least important variables and refit RF model
train_12_exclude <- train_11[ ,RF3_min_importance_11[ ,2]]
columns_include <- setdiff(names(train_11), names(train_12_exclude))
train_12 <- train_11[ ,columns_include]
names(train_12) <- make.names(names(train_12))
test_12_exclude <- test_11[ ,RF3_min_importance_11[ ,2]]
columns_include <- setdiff(names(test_11), names(test_12_exclude))
test_12 <- test_11[ ,columns_include]
names(test_12) <- make.names(names(test_12))

#Now refit the model and test the accuracy
accuracy_results_RF3_12 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_12 <- randomForest(traintarget_RF3 ~ ., data = train_12, ntree = notrees[i], 
                          proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_12, newdata = test_12)
  confusion_matrix_RF3_12 <- table(testtarget_RF3, predictions)
  accuracy_RF3_12 <- sum(diag(confusion_matrix_RF3_12)) / sum(confusion_matrix_RF3_12)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_12[i] <- accuracy_RF3_12  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_12)

#The accuracy is at 76.33% (previously 77.35%)., take another variable out.

#Identify least important factor
RF3_importance_12 <- importance(rf_3_12)
print(RF3_importance_12)

#Convert to a dataframe to identify minimum value
RF3_importance_12_df <- as.data.frame(RF3_importance_12)

#Add variable names
RF3_importance_12_df$Variable <- rownames(RF3_importance_12_df)

#Identify minimum values
RF3_min_importance_12 <- RF3_importance_12_df[order(RF3_importance_12_df$MeanDecreaseGini), ][1, ]

#Print the results
print(RF3_min_importance_12)

#Now remove the least important variables and refit RF model
train_13_exclude <- train_12[ ,RF3_min_importance_12[ ,2]]
columns_include <- setdiff(names(train_12), names(train_13_exclude))
train_13 <- train_12[ ,columns_include]
names(train_13) <- make.names(names(train_13))
test_13_exclude <- test_12[ ,RF3_min_importance_12[ ,2]]
columns_include <- setdiff(names(test_12), names(test_13_exclude))
test_13 <- test_12[ ,columns_include]
names(test_13) <- make.names(names(test_13))

#Now refit the model and test the accuracy
accuracy_results_RF3_13 <- numeric(10)  # For 10:100 in steps of 10

# Loop through each number of trees
for (i in 1:length(notrees)) {
  
  # Fit the random forest model
  rf_3_13 <- randomForest(traintarget_RF3 ~ ., data = train_13, ntree = notrees[i], 
                          proximity = TRUE)
  
  # Evaluate on the test dataset
  predictions <- predict(rf_3_13, newdata = test_13)
  confusion_matrix_RF3_13 <- table(testtarget_RF3, predictions)
  accuracy_RF3_13 <- sum(diag(confusion_matrix_RF3_13)) / sum(confusion_matrix_RF3_13)
  
  # Store the accuracy in the results vector
  accuracy_results_RF3_13[i] <- accuracy_RF3_13  # Store the accuracy at the appropriate index
}

mean(accuracy_results_RF3_13)

#The accuracy has dropped to 72.86% (previously 76.33%), which is too much of a drop.
#Therefore, reject the latest model reduction.