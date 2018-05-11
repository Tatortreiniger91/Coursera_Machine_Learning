plotFit <- function(min_x, max_x, mu, sigma, theta, p){
#PLOTFIT Plots a learned polynomial regression fit over an existing figure.
#Also works with linear regression.
#   PLOTFIT(min_x, max_x, mu, sigma, theta, p) plots the learned polynomial
#   fit with power p and feature normalization (mu, sigma).
  min_x <- min(X)
  max_x <- max(X)

# We plot a range slightly bigger than the min and max values to get
# an idea of how the fit will vary outside the range of the data points
x <- t(seq(from=(min_x - 15), to=(max_x + 25), by=0.05))

# Map the X values 
X_poly = polyFeatures(x, p)
X_poly <- matrix(mapply("-", X_poly, mu), nrow=dim(X_poly)[1], ncol=dim(X_poly)[2], byrow=T)
X_poly <- matrix(mapply("/", X_poly, sigma), nrow=dim(X_poly)[1], ncol=dim(X_poly)[2], byrow=T)


# Add ones
X_poly <- cbind(1, X_poly)

# Plot
lines(x, X_poly %*% theta, lwd=2, lty=2)
}