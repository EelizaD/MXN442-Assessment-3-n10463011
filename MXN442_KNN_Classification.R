# Load libraries
library(readr)
library(class)
library(dplyr)
library(caret)


# Prepare data ------------------------------------------------------------

#Load in the data
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
  test_data[i, ] <- dataset[end+1, ]
  train_start <- train_start + 4
  train_end <- train_end + 4
  start <- start + 5
  end <- end + 5
}

#Separate training data into the inputs and the classification
train_features <- training_data[, -c(1:3)]  #Exclude first three columns (not part of actual data)
train_labels <- training_data[, 55:57] #Class (what level bloom it is)

test_features <- test_data[, -c(1:3)]    #Same as above, but for test data
test_labels <- test_data[, 55:57] 

#Scale down the data (only the input data)
train_data_scaled <- scale(train_features[, 1:50])  # Exclude the target variable
test_data_scaled <- scale(test_features[, 1:50])      # Exclude the target variable

# Fit the RF4 model! ----------------------------------------------------------

#Set up a vector that contains the different k values used in the paper
k <- c(3, 5, 10, 20, 30)
train_labels_mod <- as.vector(train_labels[[1]]) #Convert classification to vector (4-level)
predicted_RF4 <- matrix(0, 48, 5) #Initialise matrix to store results for each k

#Create a loop that fits the model for each k value
for (i in 1:length(k)){
  predicted_RF4[ ,i] <- knn(train = train_data_scaled, 
                       test = test_data_scaled, 
                       cl = train_labels_mod, 
                       k = k[i])
}

#Now verify the accuracy for each k using a confusion matrix
test_labels_mod <- factor(as.vector(test_labels[[1]])) #Convert test classification to vector (4-level)
accuracy_results_RF4k3 <- confusionMatrix(data=factor(predicted_RF4[ ,1]), reference=test_labels_mod)
accuracy_results_RF4k5 <- confusionMatrix(data=factor(predicted_RF4[ ,2]), reference=test_labels_mod)
accuracy_results_RF4k10 <- confusionMatrix(data=factor(predicted_RF4[ ,3]), reference=test_labels_mod)
accuracy_results_RF4k20 <- confusionMatrix(data=factor(predicted_RF4[ ,4]), reference=test_labels_mod)
accuracy_results_RF4k30 <- confusionMatrix(data=factor(predicted_RF4[ ,5]), reference=test_labels_mod)

#Determine overall kappa and accuracy
kappa_RF4 <- c(accuracy_results_RF4k3[["overall"]][["Kappa"]], accuracy_results_RF4k5[["overall"]][["Kappa"]],
           accuracy_results_RF4k10[["overall"]][["Kappa"]], accuracy_results_RF4k20[["overall"]][["Kappa"]], 
           accuracy_results_RF4k30[["overall"]][["Kappa"]])
accuracy_RF4 <- c(accuracy_results_RF4k3[["overall"]][["Accuracy"]], accuracy_results_RF4k5[["overall"]][["Accuracy"]],
           accuracy_results_RF4k10[["overall"]][["Accuracy"]], accuracy_results_RF4k20[["overall"]][["Accuracy"]], 
           accuracy_results_RF4k30[["overall"]][["Accuracy"]])

kappa_avg_RF4 <- mean(kappa_RF4)
accuracy_avg_RF4 <- mean(accuracy_RF4)
kappa_avg_RF4
accuracy_avg_RF4

#Finally, plot the accuracy against the k value
plot(k, accuracy_RF4, pch=19, col="#9999FF", main="Accuracy of RF4 model for different k values", 
     xlab="k value", ylab="Accuracy of RF4 model", ylim=c(0.5, 1))


# Fit the RF3 model! ------------------------------------------------------

#Set up a vector that contains the different k values used in the paper
k <- c(3, 5, 10, 20, 30)
train_labels_mod <- as.vector(train_labels[[2]]) #Convert classification to vector (3-level)
predicted_RF3 <- matrix(0, 48, 5) #Initialise matrix to store results for each k

#Create a loop that fits the model for each k value
for (i in 1:length(k)){
  predicted_RF3[ ,i] <- knn(train = train_data_scaled, 
                            test = test_data_scaled, 
                            cl = train_labels_mod, 
                            k = k[i])
}

#Now verify the accuracy for each k using a confusion matrix
test_labels_mod <- factor(as.vector(test_labels[[2]])) #Convert test classification to vector (3-level)
accuracy_results_RF3k3 <- confusionMatrix(data=factor(predicted_RF3[ ,1]), reference=test_labels_mod)
accuracy_results_RF3k5 <- confusionMatrix(data=factor(predicted_RF3[ ,2]), reference=test_labels_mod)
accuracy_results_RF3k10 <- confusionMatrix(data=factor(predicted_RF3[ ,3]), reference=test_labels_mod)
accuracy_results_RF3k20 <- confusionMatrix(data=factor(predicted_RF3[ ,4]), reference=test_labels_mod)
accuracy_results_RF3k30 <- confusionMatrix(data=factor(predicted_RF3[ ,5]), reference=test_labels_mod)

#Determine overall kappa and accuracy
kappa_RF3 <- c(accuracy_results_RF3k3[["overall"]][["Kappa"]], accuracy_results_RF3k5[["overall"]][["Kappa"]],
               accuracy_results_RF3k10[["overall"]][["Kappa"]], accuracy_results_RF3k20[["overall"]][["Kappa"]], 
               accuracy_results_RF3k30[["overall"]][["Kappa"]])
accuracy_RF3 <- c(accuracy_results_RF3k3[["overall"]][["Accuracy"]], accuracy_results_RF3k5[["overall"]][["Accuracy"]],
                  accuracy_results_RF3k10[["overall"]][["Accuracy"]], accuracy_results_RF3k20[["overall"]][["Accuracy"]], 
                  accuracy_results_RF3k30[["overall"]][["Accuracy"]])

kappa_avg_RF3 <- mean(kappa_RF3)
accuracy_avg_RF3 <- mean(accuracy_RF3)
kappa_avg_RF3
accuracy_avg_RF3

#Finally, plot the accuracy against the k value
plot(k, accuracy_RF3, pch=19, col="#9999FF", main="Accuracy of RF3 model for different k values", 
     xlab="k value", ylab="Accuracy of RF3 model", ylim=c(0.5, 1))


# Fit the RF2 model! -------------------------------------------------------

#Set up a vector that contains the different k values used in the paper
k <- c(3, 5, 10, 20, 30)
train_labels_mod <- as.vector(train_labels[[3]]) #Convert classification to vector (2-level)
predicted_RF2 <- matrix(0, 48, 5) #Initialise matrix to store results for each k

#Create a loop that fits the model for each k value
for (i in 1:length(k)){
  predicted_RF2[ ,i] <- knn(train = train_data_scaled, 
                            test = test_data_scaled, 
                            cl = train_labels_mod, 
                            k = k[i])
}

#Now verify the accuracy for each k using a confusion matrix
test_labels_mod <- factor(as.vector(test_labels[[3]])) #Convert test classification to vector (2-level)
accuracy_results_RF2k3 <- confusionMatrix(data=factor(predicted_RF2[ ,1]), reference=test_labels_mod)
accuracy_results_RF2k5 <- confusionMatrix(data=factor(predicted_RF2[ ,2]), reference=test_labels_mod)
accuracy_results_RF2k10 <- confusionMatrix(data=factor(predicted_RF2[ ,3]), reference=test_labels_mod)
accuracy_results_RF2k20 <- confusionMatrix(data=factor(predicted_RF2[ ,4]), reference=test_labels_mod)
accuracy_results_RF2k30 <- confusionMatrix(data=factor(predicted_RF2[ ,5]), reference=test_labels_mod)

#Determine overall kappa and accuracy
kappa_RF2 <- c(accuracy_results_RF2k3[["overall"]][["Kappa"]], accuracy_results_RF2k5[["overall"]][["Kappa"]],
               accuracy_results_RF2k10[["overall"]][["Kappa"]], accuracy_results_RF2k20[["overall"]][["Kappa"]], 
               accuracy_results_RF2k30[["overall"]][["Kappa"]])
accuracy_RF2 <- c(accuracy_results_RF2k3[["overall"]][["Accuracy"]], accuracy_results_RF2k5[["overall"]][["Accuracy"]],
                  accuracy_results_RF2k10[["overall"]][["Accuracy"]], accuracy_results_RF2k20[["overall"]][["Accuracy"]], 
                  accuracy_results_RF2k30[["overall"]][["Accuracy"]])

kappa_avg_RF2 <- mean(kappa_RF2)
accuracy_avg_RF2 <- mean(accuracy_RF2)
kappa_avg_RF2
accuracy_avg_RF2

#Finally, plot the accuracy against the k value
plot(k, accuracy_RF2, pch=19, col="#9999FF", main="Accuracy of RF2 model for different k values", 
     xlab="k value", ylab="Accuracy of RF2 model", ylim=c(0.5, 1))


#Initialise a counter and a matrix to store results
counter <- 1
predicted_species <- matrix(0, 49, 45)
#Create a loop that fits each model for each of k value
for (i in 1:length(test_labels)){ #For each model
  train_labels_mod <- as.vector(train_labels[[i]]) #Convert classification to vector
  for (j in 1:length(k)){ #For each k value
    predicted_species[ ,counter] <- knn(train = train_data_scaled, 
                                        test = test_data_scaled, 
                                        cl = train_labels_mod, 
                                        k = k[j])
    counter <- counter + 1
  } 
}

#Save the predicted species to a csv file
write.csv(predicted_species, "knn_predictions.csv", row.names = TRUE)
#Save the actual classifications to a csv file
write.csv(test_labels, "actual_classifications.csv", row.names = TRUE)
