## Machine Learning Online Class
#  Exercise 5 | Regularized Linear Regression and Bias-Variance
#
#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the
#  exercise. You will need to complete the following functions:
#
#     linearRegCostFunction.m
#     learningCurve.m
#     validationCurve.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#

## Initialization
rm(list=ls())

setwd("__/Week 6")
sources <- c("linearRegCostFunction.R", "trainLinearReg.R", "learningCurve.R", "polyFeatures.R", 
             "featureNormalize.R", "plotFit.R", "validationCurve.R")
for(i in 1:length(sources)){
  source(sources[i])
}

## =========== Part 1: Loading and Visualizing Data =============
#  We start the exercise by first loading and visualizing the dataset. 
#  The following code will load the dataset into your environment and plot
#  the data.
#

# Load Training Data
print("Loading and Visualizing Data ...")

# Load from ex5data1: 
# You will have X, y, Xval, yval, Xtest, ytest in your environment
library(R.matlab)
dataset <- readMat("ex5data1.mat")
X <- dataset[["X"]]
y <- dataset[["y"]]
Xval <- dataset[["Xval"]]
yval <- dataset[["yval"]]
Xtest <- dataset[["Xtest"]]
ytest <- dataset[["ytest"]]


# m = Number of examples
m <- dim(X)[1]

# Plot training data
plot(X, y, cex=3, lwd=1.5, xlab="Change in water level (x)", ylab="Water flowing out of the dam (y)")


## =========== Part 2: Regularized Linear Regression Cost =============
#  You should now implement the cost function for regularized linear 
#  regression. 
#

theta <- c(1,1)
J <- linearRegCostFunction(cbind(1, X), y, 1)(theta)

print(paste("Cost at theta c(1,1):", J, "(this value should be about 303.993192)"))


## =========== Part 3: Regularized Linear Regression Gradient =============
#  You should now implement the gradient for regularized linear 
#  regression.
#

theta <- c(1,1)
grad <- linearRegGradFunction(cbind(1,X), y, 1)(theta)

print(paste("Gradient at theta = c(1,1):  ", grad[1], grad[2], "(this value should be about [-15.303016; 598.250744])"))

## =========== Part 4: Train Linear Regression =============
#  Once you have implemented the cost and gradient correctly, the
#  trainLinearReg function will use your cost function to train 
#  regularized linear regression.
# 
#  Write Up Note: The data is non-linear, so this will not give a great 
#                 fit.
#

#  Train linear regression with lambda = 0
lambda <- 0
theta <- trainLinearReg(cbind(1,X), y, lambda)

#  Plot fit over the data
plot(X, y, cex=3, lwd=1.5, xlab="Change in water level (x)", ylab="Water flowing out of the dam (y)")
lines(X, cbind(1, X)%*%theta, lwd=2, col="blue", lty=3)


## =========== Part 5: Learning Curve for Linear Regression =============
#  Next, you should implement the learningCurve function. 
#
#  Write Up Note: Since the model is underfitting the data, we expect to
#                 see a graph with "high bias" -- slide 8 in ML-advice.pdf 
#

lambda <- 0
error_train <- learningCurve(cbind(1,X), y, cbind(1, Xval), yval, lambda)[[1]]
error_val <- learningCurve(cbind(1,X), y, cbind(1, Xval), yval, lambda)[[2]]

plot(c(1:m,1:m),c(error_train,error_val), main="Learning curve for linear regression", xlab="Number of training examples", ylab="Error", type="n")
lines(1:m, error_train, type="l",col="blue")
lines(1:m, error_val, type="l", col="green")
legend("topright",c("Train","Cross Validation"), 
       col=c("blue","green"), lty=1)

overview <- cbind(c(1:m), error_train, error_val)
colnames(overview) <- c("Training Examples", "Train Error", "Cross Validation Error")
overview


## =========== Part 6: Feature Mapping for Polynomial Regression =============
#  One solution to this is to use polynomial regression. You should now
#  complete polyFeatures to map each example into its powers
#

p <- 8

# Map X onto Polynomial Features and Normalize
X_poly <- polyFeatures(X, 8)

X_poly <- featureNormalize(X_poly)[[1]] # Normalize
mu <- featureNormalize(X_poly)[[2]]
sigma <- featureNormalize(X_poly)[[3]]
X_poly <- cbind(1,X_poly) # Add Ones



# Map X_poly_test and normalize (using mu and sigma)
X_poly_test <- polyFeatures(Xtest, 8)
X_poly_test <- matrix(mapply("-", X_poly_test, mu), nrow=dim(X_poly_test)[1], ncol=dim(X_poly_test)[2], byrow=T)
X_poly_test <- matrix(mapply("/", X_poly_test, sigma), nrow=dim(X_poly_test)[1], ncol=dim(X_poly_test)[2], byrow=T)
X_poly_test <- cbind(1,X_poly_test) # Add Ones


# Map X_poly_val and normalize (using mu and sigma)
X_poly_val <- polyFeatures(Xtest, 8)
X_poly_val <- matrix(mapply("-", X_poly_val, mu), nrow=dim(X_poly_val)[1], ncol=dim(X_poly_val)[2], byrow=T)
X_poly_val <- matrix(mapply("/", X_poly_val, sigma), nrow=dim(X_poly_val)[1], ncol=dim(X_poly_val)[2], byrow=T)
X_poly_val <- cbind(1,X_poly_val) # Add Ones


print("Normalized Training Example 1:")
print(X_poly[1,])



## =========== Part 7: Learning Curve for Polynomial Regression =============
#  Now, you will get to experiment with polynomial regression with multiple
#  values of lambda. The code below runs polynomial regression with 
#  lambda = 0. You should try running the code with different values of
#  lambda to see how the fit and learning curve change.
#

lambda <- 1
theta <- trainLinearReg(X_poly, y, lambda)


# Plot training data and fit
plot(X, y, cex=3, lwd=1.5, xlab="Change in water level (x)", ylab="Water flowing out of the dam (y)", main=c("Polynomial Regression Fit lambda =", lambda))


plotFit(min(X), max(X), mu, sigma, theta, p)


error_train <- learningCurve(X_poly, y, X_poly_val, yval, lambda)[[1]]
error_val <- learningCurve(X_poly, y, X_poly_val, yval, lambda)[[2]]


plot(c(1:m,1:m),c(error_train,error_val), main="Learning curve for linear regression", xlab="Number of training examples", ylab="Error", type="n")
lines(1:m, error_train, type="l",col="blue")
lines(1:m, error_val, type="l", col="green")
legend("topright",c("Train","Cross Validation"), 
       col=c("blue","green"), lty=1)

overview <- cbind(c(1:m), error_train, error_val)
colnames(overview) <- c("Training Examples", "Train Error", "Cross Validation Error")
overview


## =========== Part 8: Validation for Selecting Lambda =============
#  You will now implement validationCurve to test various values of 
#  lambda on a validation set. You will then use this to select the
#  "best" lambda value.
#
lambda_vec <- validationCurve(X_poly, y, X_poly_val, yval)[[1]]
error_train <- validationCurve(X_poly, y, X_poly_val, yval)[[2]]
error_val <- validationCurve(X_poly, y, X_poly_val, yval)[[3]]

plot(c(1:length(error_train),1:length(error_val)),c(error_train,error_val), xlab="lambda", ylab="Error", type="n")
lines(1:length(error_train), error_train, type="l",col="blue")
lines(1:length(error_val), error_val, type="l", col="green")
legend("topleft",c("Train","Cross Validation"), 
       col=c("blue","green"), lty=1)


overview <- cbind(c(1:m), error_train, error_val)
colnames(overview) <- c("Training Examples", "Train Error", "Cross Validation Error")
overview
