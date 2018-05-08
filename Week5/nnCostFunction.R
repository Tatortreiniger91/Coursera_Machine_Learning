nnCostFunction <- function(nn_params, 
                           input_layer_size, 
                           hidden_layer_size, 
                           num_labels, 
                           X, y, lambda){
#NNCOSTFUNCTION Implements the neural network cost function for a two layer
#neural network which performs classification
#   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
#   X, y, lambda) computes the cost and gradient of the neural network. The
#   parameters for the neural network are "unrolled" into the vector
#   nn_params and need to be converted back into the weight matrices. 
# 
#   The returned parameter grad should be a "unrolled" vector of the
#   partial derivatives of the neural network.
#
function(nn_params){
# Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
# for our 2 layer neural network
Theta1 <- matrix(nn_params[1:(hidden_layer_size * (input_layer_size + 1))],
                hidden_layer_size, (input_layer_size + 1))
Theta2 <- matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)], 
                 num_labels, (hidden_layer_size + 1))

# Setup some useful variables
m <- dim(X)[1]

# You need to return the following variables correctly 
J <- 0
Theta1_grad <- matrix(0, length(Theta1), 1)
Theta2_grad <- matrix(0, length(Theta2), 1)

# ====================== YOUR CODE HERE ======================
# Instructions: You should complete the code by working through the
#               following parts.
#
# Part 1: Feedforward the neural network and return the cost in the
#         variable J. After implementing Part 1, you can verify that your
#         cost function computation is correct by verifying the cost
#         computed in ex4.m


# y(k) - the great trick - we need to recode the labels as vectors containing only values 0 or 1 (page 5 of ex4.pdf)
diagonal_ones <- diag(num_labels)
y_new <- matrix(0, m, num_labels)
for (i in 1:m){
  y_new[i,] <- diagonal_ones[y[i],]
}

# Foward propagation

a1 <- cbind(rep(1, m), X)
z2 <- a1 %*% t(Theta1)
a2 <- g(z2)
a2 <- cbind(matrix(rep(1,m)), a2)

z3 <- a2 %*% t(Theta2)
a3 <- g(z3)



# Note we should not regularize the terms that correspond to the bias. 
# For the matrices Theta1 and Theta2, this corresponds to the first column of each matrix.
t1 <- Theta1[,2:dim(Theta1)[2]]
t2 <- Theta2[,2:dim(Theta2)[2]]

# J function

J <- (1/m) * sum(sum((-y_new) * log(a3) - (1-y_new) * log(1-a3)))

# Regularization
Reg <- lambda  * (sum(sum( t1^ 2 )) + sum( t2^ 2 )) / (2*m)

# Regularized cost function
J <- J + Reg
return(J)
}
}
  
# Part 2: Implement the backpropagation algorithm to compute the gradients
#         Theta1_grad and Theta2_grad. You should return the partial derivatives of
#         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
#         Theta2_grad, respectively. After implementing Part 2, you can check
#         that your implementation is correct by running checkNNGradients
#
#         Note: The vector y passed into the function is a vector of labels
#               containing values from 1..K. You need to map this vector into a 
#               binary vector of 1's and 0's to be used with the neural network
#               cost function.
#
#         Hint: We recommend implementing backpropagation using a for-loop
#               over the training examples if you are implementing it for the 
#               first time.
nnGradFunction <- function(nn_params, 
                           input_layer_size, 
                           hidden_layer_size, 
                           num_labels, 
                           X, y, lambda){
  function(nn_params){
  # Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
  # for our 2 layer neural network
  Theta1 <- matrix(nn_params[1:(hidden_layer_size * (input_layer_size + 1))],
                   hidden_layer_size, (input_layer_size + 1))
  Theta2 <- matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)], 
                   num_labels, (hidden_layer_size + 1))
  
  # Setup some useful variables
  m <- dim(X)[1]
  
  # You need to return the following variables correctly 
  Theta1_grad <- matrix(0, length(Theta1), 1)
  Theta2_grad <- matrix(0, length(Theta2), 1)
  
  diagonal_ones <- diag(num_labels)
  y_new <- matrix(0, m, num_labels)
  for (i in 1:m){
    y_new[i,] <- diagonal_ones[y[i],]
  }

  # Back propagation
  a1 <- cbind(rep(1, m), X)
  z2 <- a1 %*% t(Theta1)
  a2 <- cbind(rep(1,dim(z2)[1]), g(z2))
  z3 <- a2 %*% t(Theta2)
  a3 <- g(z3)
  
  
  delta_3 <- a3 - y_new
  z2 <- cbind(rep(1, m), z2)
  
  delta_2 <- (delta_3 %*% Theta2) * sigmoidGradient(z2)
  delta_2 <- delta_2[,2:ncol(delta_2)]
  
  
  Theta2_grad <- Theta2_grad + c(t(delta_3) %*% a2)
  Theta1_grad <- Theta1_grad + c(t(delta_2) %*% a1)
  
  
  Theta2_grad <- (1/m) * Theta2_grad
  Theta1_grad <- (1/m) * Theta1_grad


# Part 3: Implement regularization with the cost function and gradients.
#
#         Hint: You can implement this around the code for
#    backpropagation. That is, you can compute the gradients for
#    the regularization separately and then add them to Theta1_grad
#    and Theta2_grad from Part 2.

# Regularization

Theta1_grad <- (1/m) * Theta1_grad + (lambda/m) * c(c(matrix(rep(0, dim(Theta1)[1]), 1)), c(Theta1[,2:ncol(Theta1)]))
Theta2_grad <- (1/m) * Theta2_grad + (lambda/m) * c(c(matrix(rep(0, dim(Theta2)[1]), 1)), c(Theta2[,2:ncol(Theta2)]))


# Unroll gradients
grad <- c(Theta1_grad, Theta2_grad)
return(grad)
}
}
