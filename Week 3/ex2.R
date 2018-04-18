## Machine Learning Online Class - Exercise 2: Logistic Regression
#
#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the logistic
#  regression exercise. You will need to complete the following functions 
#  in this exericse:
#
#     sigmoid.R
#     costFunction.R
#     predict.R
#     costFunctionReg.R
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#

## Initialization
rm(list=ls())

setwd("C:/Users/AVogl/Desktop/Coursera/Week 3")
sources <- c("plotData.R", "sigmoid.R","costFunction.R", "grad.R",
             "predict.R","mapFeature.R", "plotDecisionBoundary.R")
for(i in 1:length(sources)){
  source(sources[i])
}


## Load Data
#  The first two columns contains the exam scores and the third column
#  contains the label.

data <- read.table("ex2data1.txt", sep=",")
X <- data[,1:2]; y <- data[,3]

## ==================== Part 1: Plotting ====================
#  We start the exercise by first plotting the data to understand the 
#  the problem we are working with.

print("Plotting data with indicating (y = 1) examples and indicating (y = 0) examples.")

plotData(X, y)

# Specified in plot order
legend("topright", c("Admitted", "Not admitted"), pch=c("+", "o"), col=c("red","blue"))


## ============ Part 2: Compute Cost and Gradient ============
#  In this part of the exercise, you will implement the cost and gradient
#  for logistic regression. You neeed to complete the code in 
#  costFunction.m

#  Setup the data matrix appropriately, and add ones for the intercept term
m <- dim(X)[1]
n <- dim(X)[2]

# Add intercept term to x and X_test
X <- matrix(c(rep(1,m), X[,1], X[,2]), nrow=m)

# Initialize fitting parameters
initial_theta <- matrix(0, nrow = (n+1))

# Compute and display initial cost and gradient
cost <- costFunction(X, y)(initial_theta)
grd <- grad(X, y)(initial_theta)

print(paste("Cost at initial theta (zeros): ", cost))


print("Gradient at initial theta (zeros):");
print(grd)


## ============= Part 3: Optimizing using optim  =============
#  In this exercise, you will use a built-in function (optim) to find the
#  optimal parameters theta.


#  Run optim to obtain the optimal theta
#  This function will return theta and the cost 
optimization <- optim(par=c(initial_theta), fn=costFunction(X,y), gr=grad(X,y), 
                  method="BFGS", control = list(maxit = 400))

theta <- as.matrix(optimization$par)
costnew <- optimization$value


# Print theta to screen
print(paste("Cost at theta found by fminunc: ", costnew))
print("theta:")
print(theta)

# Plot Boundary
plotDecisionBoundary(theta, X, y)


## ============== Part 4: Predict and Accuracies ==============
#  After learning the parameters, you"ll like to use it to predict the outcomes
#  on unseen data. In this part, you will use the logistic regression model
#  to predict the probability that a student with score 45 on exam 1 and 
#  score 85 on exam 2 will be admitted.
#
#  Furthermore, you will compute the training and test set accuracies of 
#  our model.
#
#  Your task is to complete the code in predict.m

#  Predict probability for a student with score 45 on exam 1 
#  and score 85 on exam 2 
prob <- g(c(1, 45, 85)%*%theta)

print(paste("For a student with scores 45 and 85, we predict an admission probability of ", prob))

# Compute accuracy on our training set
p <- predict2(theta, X)

print(paste("Train Accuracy:", mean(p == y) * 100))
