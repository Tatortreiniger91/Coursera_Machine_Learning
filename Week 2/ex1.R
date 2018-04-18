## Machine Learning Online Class - Exercise 1: Linear Regression

#  Instructions
#  ------------
# 
#  This file contains code that helps you get started on the
#  linear exercise. You will need to complete the following functions 
#  in this exericse:
#
#     warmUpExercise.m
#     plotData.m
#     gradientDescent.m
#     computeCost.m
#     gradientDescentMulti.m
#     computeCostMulti.m
#     featureNormalize.m
#     normalEqn.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#
# x refers to the population size in 10,000s
# y refers to the profit in $10,000s
#

## Initialization
rm(list=ls())
setwd("path/Coursera/Week 2")
sources <- c("computeCost.R","gradientDescent.R",
             "plotData.R","warmUpExercise.R")
for(i in 1:length(sources)){
  source(sources[i])
}
## ==================== Part 1: Basic Function ====================
# Complete warmUpExercise.m 
print('Running warmUpExercise ...')
print('5x5 Identity Matrix:')
warmUpExercise()

## ======================= Part 2: Plotting =======================
print('Plotting Data ...')
data <- read.table("ex1data1.txt", sep=",")
X <- data[,1]; y <- data[,2]
m<- length(y) # number of training examples

# Plot Data
# Note: You have to complete the code in plotData.m
plotData(X, y)

## =================== Part 3: Gradient descent ===================
print('Running Gradient Descent ...')

X <- matrix(c(rep(1,m), data[,1]), ncol=2, nrow=m) # Add a column of ones to x
theta <- matrix(c(0,0), ncol=1, nrow=2) # initialize fitting parameters

# Some gradient descent settings
iterations <- 1500
alpha <- 0.01


# compute and display initial cost
computeCost(X, y, theta)

# run gradient descent
theta <- gradientDescent(X, y, theta, alpha, iterations)

# print theta to screen
print('Theta found by gradient descent: ')
print(theta$theta)
# Plot the linear fit
lines(X[,2], X  %*% theta$theta,col="blue")

# Predict values for population sizes of 35,000 and 70,000
predict1 <- c(1, 3.5)%*%theta$theta
print(paste('For population = 35,000, we predict a profit of', predict1*10000))
predict2 = c(1, 7)%*%theta$theta
print(paste('For population = 70,000, we predict a profit of', predict2*10000))

######################################################################
## ============= Part 4: Visualizing J(theta_0, theta_1) =============
print('Visualizing J(theta_0, theta_1) ...\n')

# Grid over which we will calculate J
theta0_vals <- seq(-10,10,length=100)
theta1_vals <- seq(-1,4,length=100)

# initialize J_vals to a matrix of 0's
J_vals = matrix(0, nrow=length(theta0_vals), ncol=length(theta1_vals));

# Fill out J_vals
for(i in 1:length(theta0_vals)){
  for(j in 1:length(theta1_vals)){
    t <- c(theta0_vals[i], theta1_vals[j])
    J_vals[i,j] <- computeCost(X, y, t)
  }
}



# Because of the way meshgrids work in the surf command, we need to 
# transpose J_vals before calling surf, or else the axes will be flipped
J_vals <- t(J_vals)
# Surface plot
windows()
persp(theta0_vals,theta1_vals,J_vals,
      theta=30, phi=30, expand=0.6,
      col='lightblue', shade=0.75, ltheta=120,
      ticktype='detailed', xlab="theta_0", ylab="theta_1")

# Contour plot
# Plot J_vals as 15 contours spaced logarithmically between 0.01 and 100
logspace <- function( d1, d2, n){
  return(exp(log(10)*seq(d1, d2, length.out=n)))
}
contour(theta0_vals, theta1_vals, J_vals, levels=logspace(-2,3, 20), xlab="theta_0", ylab="theta_1")
points(theta$theta[1], theta$theta[2], col = "red", pch = 4,cex = 1.1,lwd = 2)
