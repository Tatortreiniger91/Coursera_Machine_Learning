sigmoidGradient <- function(z){
  #SIGMOIDGRADIENT returns the gradient of the sigmoid function
  #evaluated at z
  #   G = SIGMOIDGRADIENT(z) computes the gradient of the sigmoid function
  #   evaluated at z. This should work regardless if z is a matrix or a
  #   vector. In particular, if z is a vector or matrix, you should return
  #   the gradient for each element.
  
  #G <- matrix(0, dim(z)[1], 1)
  
  # ====================== YOUR CODE HERE ======================
  # Instructions: Compute the gradient of the sigmoid function evaluated at
  #               each value of z (z can be a matrix, vector or scalar).
  
  
  G <- g(z) * (1 - g(z));
  
  # =============================================================
}
