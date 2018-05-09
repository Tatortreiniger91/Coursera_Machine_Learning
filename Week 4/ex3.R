## Machine Learning Online Class - Exercise 3 | Part 1: One-vs-all

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

setwd("___/Week 4")
sources <- c("sigmoid.R", "displayData.R", "costFunction.R", "oneVsAll.R", "predictOneVsAll.R")
for(i in 1:length(sources)){
  source(sources[i])
}

## Setup the parameters you will use for this part of the exercise
input_layer_size <- 400  # 20x20 Input Images of Digits
num_labels <- 10          # 10 labels, from 1 to 10   
# (note that we have mapped "0" to label 10)

## =========== Part 1: Loading and Visualizing Data =============
#  We start the exercise by first loading and visualizing the dataset. 
#  You will be working with a dataset that contains handwritten digits.

# Load Training Data
print("Loading and Visualizing Data ...")

library(R.matlab)
dataset <- readMat("ex3data1.mat")
X <- dataset[["X"]]
y <- dataset[["y"]] # training data stored in arrays X, y


m <- dim(X)[1]  


# Randomly select 100 data points to display
rand_indices <- sample(1:m, m, replace=FALSE) # Array of size 1*5000 having random no. at each positions
sel <- X[rand_indices[1:100], ]

displayData(sel)


## ============ Part 2: Vectorize Logistic Regression ============
#  In this part of the exercise, you will reuse your logistic regression
#  code from the last exercise. You task here is to make sure that your
#  regularized logistic regression implementation is vectorized. After
#  that, you will implement one-vs-all classification for the handwritten
#  digit dataset.
#

print("Training One-vs-All Logistic Regression...")

lambda <- 0.1
# each X row corresponds to training data digit image 20*20 pixel
# each y row contains label for the training data i.e. actual name of the
# digit
all_theta <- oneVsAll(X, y, num_labels, lambda)



## ================ Part 3: Predict for One-Vs-All ================
#  After ...
pred <- predictOneVsAll(all_theta, X) # all_theta 10 * 401 mat

print(paste("Training Set Accuracy: ", mean(pred == y) * 100, "Percent"))

#additional: Accuracy per number
zero <- mean(pred[1:500] == y[1:500]) * 100
one <- mean(pred[501:1000] == y[501:1000]) * 100
two <- mean(pred[1001:1500] == y[1001:1500]) * 100
three <- mean(pred[1501:2000] == y[1501:2000]) * 100
four <- mean(pred[2001:2500] == y[2001:2500]) * 100
five <- mean(pred[2501:3000] == y[2501:3000]) * 100
six <- mean(pred[3001:3500] == y[3001:3500]) * 100
seven <- mean(pred[3501:4000] == y[3501:4000]) * 100
eight <- mean(pred[4001:4500] == y[4001:4500]) * 100
nine <- mean(pred[4501:5000] == y[4501:5000]) * 100
zero; one; two; three; four; five; six; seven; eight; nine

# the prediction of 6 and 9 is very bad
# the prediction of 3, 4, 7 and 8 is good
# the prediction of 0 is almost perfect
