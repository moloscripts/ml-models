library("tidyverse")
library(rpart) # Regression Trees
library(randomForest) 
library(modelr) # For getting MAE
library(glue)

melbourne_data <- read_csv("data/melb_data.csv") 
Filter(is.integer, melbourne_data)
melbourne_data$Rooms <- suppressWarnings(as.numeric(as.integer(melbourne_data$Rooms)))
melbourne_data <- na.omit(melbourne_data)
summary(melbourne_data)

# Prediction Target ~ Predictor1 + Predictor2 + Predictor3...
# Define our model
mel_fit <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea + YearBuilt + Lattitude + Longtitude, data = melbourne_data)

# plot regression tree
plot(mel_fit, uniform = TRUE)
text(mel_fit, cex=.6) # .6 means to make the text 60% big. The normal size is very big and clustered

# Use the predict() function to predict houses based on our model we created
# Select 5 houses (training data) to perform a prediction and get it's MAE - This is called the in-sample score.
print(head(melbourne_data))

print("Predictions of the 5 houses selected are: ")
print(predict(mel_fit, head(melbourne_data)))

print("The Actual price of the Houses are: ")
print(head(melbourne_data$Price))

# Getting the MEAN Absolute Error (MAE)
mel_MAE <- mae(model = mel_fit, data = melbourne_data).

# In-Sample score represents a very small % of the whole data. Thefore this prediction cannot be used to rep the bigger picture
# Split our DF to testing and validating/training data. 
splitData <- resample_partition(melbourne_data, c(test = 0.3, train = 0.7))
lapply(splitData, dim)

# fit our model using training data and test it using testing data

# fit the new model using trainig data then test it using the testing data
mel_fit2 <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea + YearBuilt + Lattitude + Longtitude, data = splitData$train)
mae(model = mel_fit2, data = splitData$test) # MAE of the new model based on test data

# improve the accuracy of our model, we'll rty to control the max depth, get the 'sweet spot' btn underfitting and overfitting. 
# get the max average error for a given max depth or the height of the tree.

get_mae <- function(maxdepth, target, predictors, training_data, testing_data) {
  predictors <- paste(predictors, collapse = "+")
  formular <- as.formula(paste(target, "~", predictors, sep = " "))
  
  # build our model
  model <- rpart(formular, data = training_data, control = rpart.control(maxdepth = maxdepth))
  # get mae
  mae <- mae(model, testing_data)
  return(mae)
}

target <- "Price"
predictors <- c("Rooms","Bathroom","Landsize","BuildingArea", "YearBuilt","Lattitude","Longtitude")
training_data <- splitData$train
testing_data <- splitData$test

for(i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predictors, training_data = training_data, testing_data = testing_data)
  print(glue::glue("Maxdepth: ",i,"\t MAE ", mae))
}








