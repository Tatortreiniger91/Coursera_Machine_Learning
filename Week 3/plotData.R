
#PLOTDATA Plots the data points X and y into a new figure 
#   PLOTDATA(x,y) plots the data points with + for the positive examples
#   and o for the negative examples. X is assumed to be a Mx2 matrix.

# Create New Figure

# ====================== YOUR CODE HERE ======================
# Instructions: Plot the positive and negative examples on a
#               2D plot, using the option 'k+' for the positive
#               examples and 'ko' for the negative examples.
#

plotData <- function(X, y){
# Find Indices of Positive and Negative Examples
pos <- which(y == 1)
neg <- which(y == 0)
windows()
plot(X[pos, 1], X[pos, 2], pch="+", lwd=2, cex=3, col="red", xlab="Exam 1 score", ylab="Exam 2 score")
points(X[neg, 1], X[neg, 2], pch="o", lwd=2, cex=3, col="blue")
}


# =========================================================================