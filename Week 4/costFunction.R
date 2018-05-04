costFunc <- function(X, y, lambda){
#COSTFUNCTION Compute cost and gradient for logistic regression
#   J = COSTFUNCTION(theta, X, y) computes the cost of using theta as the
#   parameter for logistic regression and the gradient of the cost
#   w.r.t. to the parameters.


# ====================== YOUR CODE HERE ======================
# Instructions: Compute the cost of a particular choice of theta.
#               You should set J to the cost.
#               Compute the partial derivatives and set grad to the partial
#               derivatives of the cost w.r.t. each parameter in theta
#
# Note: grad should have the same dimensions as theta
#
  function(theta){
    m <- length(y)
    J <- 0
    hyp <- g(X %*% theta)
    theta1 <- c(0,c(theta[-1]))
    
    J <- (t(-y) %*% log(hyp) - t(-1 + y) %*% log(1 - hyp))/m + lambda * (t(theta1) %*% theta1) / (2*m)
    return(J)
  }
}

gradFunc <- function(X, y, lambda){
  function(theta){
    m <- length(y)
    grad <- matrix(0,nrow=length(theta))
    
    hyp <- g(X %*% theta)
    theta1 <- c(0,c(theta[-1]))
    
    grad <- (t(X) %*% (hyp - y) + lambda * theta1) / m
    return(grad)
  }
}


# =============================================================
