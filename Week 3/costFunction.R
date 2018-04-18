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
costFunction <- function(X, y){
  function(theta){
    m <- length(y)
    J <- 0
    J <- (t(-y)%*%log(g(X%*%theta)) - t(1-y)%*%log(1-g(X%*%theta)))/nrow(X)
    return(list(cost=J))
  }
}


# =============================================================
