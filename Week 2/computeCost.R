#Week 2

#COMPUTECOST Compute cost for linear regression
#   J = COMPUTECOST(X, y, theta) computes the cost of using theta as the
#   parameter for linear regression to fit the data points in X and y


# You need to return the following variables correctly 


# ====================== YOUR CODE HERE ======================
# Instructions: Compute the cost of a particular choice of theta
#               You should set J to the cost.



computeCost <- function(X, y, theta){
  m <- length(y)
  J <- 0
  J <- (1/(2*m))*sum((X%*%theta - y)^2)
  return(J)
}
