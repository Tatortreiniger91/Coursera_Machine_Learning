## Machine Learning Online Class - Exercise 3 | Part 2: Neural Networks

#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the
#  linear exercise. You will need to complete the following functions 
#  in this exericse:
#
#     lrCostFunction.m (logistic regression cost function)
#     oneVsAll.m
#     predictOneVsAll.m
#     predict.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#

## Initialization
rm(list=ls())


setwd("C:/Users/AVogl/Desktop/Coursera/Week 4")
sources <- c("sigmoid.R", "displayData.R", "costFunction.R", "oneVsAll.R", "predictOneVsAll.R", "PREDICT.R")
for(i in 1:length(sources)){
  source(sources[i])
}

## Setup the parameters you will use for this exercise
input_layer_size <- 400  # 20x20 Input Images of Digits
hidden_layer_size <- 25   # 25 hidden units
num_labels <- 10          # 10 labels, from 1 to 10   
# (note that we have mapped "0" to label 10)

## =========== Part 1: Loading and Visualizing Data =============
#  We start the exercise by first loading and visualizing the dataset. 
#  You will be working with a dataset that contains handwritten digits.
#

# Load Training Data
print("Loading and Visualizing Data ...")

library(R.matlab)
dataset <- readMat("C:/Users/AVogl/Desktop/Coursera/Week 4/ex3data1.mat")
X <- dataset[["X"]]
y <- dataset[["y"]] # training data stored in arrays X, y
m <- dim(X)[1]

# Randomly select 100 data points to display
rand_indices <- sample(1:m, m, replace=FALSE)
sel <- X[rand_indices[1:100], ]

displayData(sel)


## ================ Part 2: Loading Pameters ================
# In this part of the exercise, we load some pre-initialized 
# neural network parameters.

print("Loading Saved Neural Network Parameters ...")

# Load the weights into variables Theta1 and Theta2
thetaset <- readMat("C:/Users/AVogl/Desktop/Coursera/Week 4/ex3weights.mat")
Theta1 <- thetaset[["Theta1"]]
Theta2 <- thetaset[["Theta2"]]

## ================= Part 3: Implement Predict =================
#  After training the neural network, we would like to use it to predict
#  the labels. You will now implement the "predict" function to use the
#  neural network to predict the labels of the training set. This lets
#  you compute the training set accuracy.

pred <- PREDICT(Theta1, Theta2, X)

print(paste("Training Set Accuracy: ", mean((pred == y)) * 100, "Percent"))


#  To give you an idea of the network's output, you can also run
#  through the examples one at the a time to see what it is predicting.

#  Randomly permute examples
rp <- sample(1:m, m, replace=FALSE)

displayData(X[rp,])
pred <- PREDICT(Theta1, Theta2, X[rp,])
print(paste0("Neural Network Prediction ", pred, "; Actual y ", y[rp], "; Number ", pred%%10))

