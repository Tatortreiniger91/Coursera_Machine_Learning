linearRegCostFunction <- function(X, y, lambda){
#LINEARREGCOSTFUNCTION Compute cost and gradient for regularized linear 
#regression with multiple variables
#   [J, grad] = LINEARREGCOSTFUNCTION(X, y, theta, lambda) computes the 
#   cost of using theta as the parameter for linear regression to fit the 
#   data points in X and y. Returns the cost in J and the gradient in grad

  function(theta){
  # Initialize some useful values
  m <- length(y) # number of training examples

  # You need to return the following variables correctly 
  J <- 0
  
  h_theta <- X %*% theta

  J <- 1/(2*m) * t(h_theta - y) %*% (h_theta - y) + (lambda/(2*m)) * t(theta[2:length(theta)]) %*% theta[2:length(theta)]
  return(J)
  }
}

linearRegGradFunction <- function(X, y, lambda){
  
  function(theta){
  m <- length(y)
  grad <- rep(0, length(theta))
  
  h_theta <- X %*% theta
  
  thetaZero <- theta
  thetaZero[1] <- 0

  grad <- (1 / m) * (t(X) %*% (h_theta - y)) + lambda / m * thetaZero
  return(grad)
  }
}
