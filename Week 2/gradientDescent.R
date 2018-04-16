#GRADIENTDESCENT Performs gradient descent to learn theta
#   theta = GRADIENTDESENT(X, y, theta, alpha, num_iters) updates theta by 
#   taking num_iters gradient steps with learning rate alpha




# ====================== YOUR CODE HERE ======================
# Instructions: Perform a single gradient step on the parameter vector
#               theta. 
#
# Hint: While debugging, it can be useful to print out the values
#       of the cost function (computeCost) and gradient here.
#

gradientDescent <- function(X, y, theta, alpha, num_iters){
  m <- length(y)
  J_hist <- rep(0,1500)
  for(i in 1:1500){
    delta <- (1/m)* (t(X)%*%X%*%theta - t(X)%*%y)
    theta <- (theta - (alpha * delta))
    J_hist[i] <- computeCost(X, y, theta)
  }
  result <- list(theta=theta, J_hist=J_hist)
  return(result)
}

