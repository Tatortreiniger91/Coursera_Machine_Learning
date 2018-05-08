## Machine Learning Online Class - Exercise 4 Neural Network Learning

#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the
#  linear exercise. You will need to complete the following functions 
#  in this exericse:
#
#     sigmoidGradient.m
#     randInitializeWeights.m
#     nnCostFunction.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#

## Initialization
rm(list=ls())


setwd("C:/Users/AVogl/Desktop/Coursera/Week 5")
sources <- c("sigmoid.R", "displayData.R", "sigmoidGradient.R", "nnCostFunction.R", "checkNNGradients.R", 
             "computeNumericalGradient.R", "debugInitializeWeights.R", "randInitializeWeights.R", "PREDICT.R")
for(i in 1:length(sources)){
  source(sources[i])
}


## Setup the parameters you will use for this exercise
input_layer_size <- 400   # 20x20 Input Images of Digits
hidden_layer_size <- 25   # 25 hidden units
num_labels <- 10          # 10 labels, from 1 to 10   
# (note that we have mapped "0" to label 10)

## =========== Part 1: Loading and Visualizing Data =============
#  We start the exercise by first loading and visualizing the dataset. 
#  You will be working with a dataset that contains handwritten digits.

# Load Training Data
print("Loading and Visualizing Data ...")

library(R.matlab)
dataset <- readMat("C:/Users/AVogl/Desktop/Coursera/Week 5/ex4data1.mat")
X <- dataset[["X"]]
y <- dataset[["y"]]

m <- dim(X)[1]

# Randomly select 100 data points to display
rand_indices <- sample(1:m, m, replace=FALSE) # Array of size 1*5000 having random no. at each positions
sel <- X[rand_indices[1:100], ]

displayData(sel)



## ================ Part 2: Loading Parameters ================
# In this part of the exercise, we load some pre-initialized 
# neural network parameters.

print("Loading Saved Neural Network Parameters ...")

# Load the weights into variables Theta1 and Theta2
Theta <- readMat("C:/Users/AVogl/Desktop/Coursera/Week 5/ex4weights.mat")
Theta1 <- Theta[["Theta1"]]
Theta2 <- Theta[["Theta2"]]

# Unroll parameters 
nn_params <- c(c(Theta1), c(Theta2))

## ================ Part 3: Compute Cost (Feedforward) ================
#  To the neural network, you should first start by implementing the
#  feedforward part of the neural network that returns the cost only. You
#  should complete the code in nnCostFunction.m to return cost. After
#  implementing the feedforward to compute the cost, you can verify that
#  your implementation is correct by verifying that you get the same cost
#  as us for the fixed debugging parameters.
#
#  We suggest implementing the feedforward cost *without* regularization
#  first so that it will be easier for you to debug. Later, in part 4, you
#  will get to implement the regularized cost.
#
print("Feedforward Using Neural Network ...")

# Weight regularization parameter (we set this to 0 here)

lambda <- 0

J <- nnCostFunction(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)(nn_params)

print(paste("Cost at parameters (loaded from ex4weights):", J, "(this value should be about 0.287629)"))

#####################################################################
## =============== Part 4: Implement Regularization ===============
#  Once your cost function implementation is correct, you should now
#  continue to implement the regularization with the cost.
#

print("Checking Cost Function (w/ Regularization) ... ")

# Weight regularization parameter (we set this to 1 here).
lambda <- 1

J <- nnCostFunction(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)(nn_params)

print(paste("Cost at parameters (loaded from ex4weights):", J, "(this value should be about 0.383770)"))


## ================ Part 5: Sigmoid Gradient  ================
#  Before you start implementing the neural network, you will first
#  implement the gradient for the sigmoid function. You should complete the
#  code in the sigmoidGradient.m file.
#

print("Evaluating sigmoid gradient...")

g_eval <- sigmoidGradient(c(1, -0.5, 0, 0.5, 1))
print("Sigmoid gradient evaluated at c(1, -0.5, 0, 0.5, 1): ")
print(g_eval)


## ================ Part 6: Initializing Pameters ================
#  In this part of the exercise, you will be starting to implment a two
#  layer neural network that classifies digits. You will start by
#  implementing a function to initialize the weights of the neural network
#  (randInitializeWeights.m)

print("Initializing Neural Network Parameters ...")

initial_Theta1 <- randInitializeWeights(input_layer_size, hidden_layer_size)
initial_Theta2 <- randInitializeWeights(hidden_layer_size, num_labels)

# Unroll parameters
initial_nn_params = c(c(initial_Theta1), c(initial_Theta2))


## =============== Part 7: Implement Backpropagation ===============
#  Once your cost matches up with ours, you should proceed to implement the
#  backpropagation algorithm for the neural network. You should add to the
#  code you've written in nnCostFunction.m to return the partial
#  derivatives of the parameters.
#
print("Checking Backpropagation... ")

#  Check gradients by running checkNNGradients
checkNNGradients()



## =============== Part 8: Implement Regularization ===============
#  Once your backpropagation implementation is correct, you should now
#  continue to implement the regularization with the cost and gradient.
#

print("Checking Backpropagation (w/ Regularization) ... ")

#  Check gradients by running checkNNGradients
lambda <- 3
checkNNGradients(lambda);

# Also output the costFunction debugging values
debug_J  = nnCostFunction(nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)(nn_params)

print(paste("Cost at (fixed) debugging parameters (w/ lambda = 3):", debug_J, "(this value should be about 0.576051)"))


## =================== Part 8: Training NN ===================
#  You have now implemented all the code necessary to train a neural 
#  network. To train your neural network, we will now use "fmincg", which
#  is a function which works similarly to "fminunc". Recall that these
#  advanced optimizers are able to train our cost functions efficiently as
#  long as we provide them with the gradient computations.
#
print("Training Neural Network... ")

#  You should also try different values of lambda
lambda <- 1

# Create "short hand" for the cost function to be minimized
costFunction <- nnCostFunction(initial_nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)

# Create "short hand" for the grad function to be minimized
gradFunction <- nnGradFunction(initial_nn_params, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)

# Now, costFunction is a function that takes in only one argument (the
# neural network parameters)
opt <- optim(initial_nn_params, fn= costFunction, gr=gradFunction,
             control = list(trace=1,maxit=50))

opt$par
nn_params <- opt$par
cost <- opt$value

# Obtain Theta1 and Theta2 back from nn_params
Theta1 <- matrix(nn_params[1:(hidden_layer_size * (input_layer_size + 1))],
                 hidden_layer_size, (input_layer_size + 1))
Theta2 <- matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)], 
                 num_labels, (hidden_layer_size + 1))


## ================= Part 9: Visualize Weights =================
#  You can now "visualize" what the neural network is learning by
#  displaying the hidden units to see what features they are capturing in 
#  the data.

print("Visualizing Neural Network... ")

displayData(Theta1[, 2:ncol(Theta1)])

## ================= Part 10: Implement Predict =================
#  After training the neural network, we would like to use it to predict
#  the labels. You will now implement the "predict" function to use the
#  neural network to predict the labels of the training set. This lets
#  you compute the training set accuracy.

pred <- PREDICT(Theta1, Theta2, X)

print(paste("Training Set Accuracy: ", mean(pred == y) * 100))

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

#very high accuracy for each number
