trainLinearReg <- function(X, y, lambda){
#TRAINLINEARREG Trains linear regression given a dataset (X, y) and a
#regularization parameter lambda
#   [theta] = TRAINLINEARREG (X, y, lambda) trains linear regression using
#   the dataset (X, y) and regularization parameter lambda. Returns the
#   trained parameters theta.
#
if (class(X) == "numeric")
  X <- t(X)

# Initialize Theta
initial_theta <- rep(0, dim(X)[2])

costFunction <- linearRegCostFunction(X, y, lambda)
gradFunction <- linearRegGradFunction(X, y, lambda)

# Minimize using optim
opt <- optim(par = initial_theta, fn = costFunction, gr = gradFunction, method = "BFGS",
             control = list(trace=1,maxit=50))
opt$par
}