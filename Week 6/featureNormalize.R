featureNormalize <- function(X){
#FEATURENORMALIZE Normalizes the features in X 
#   FEATURENORMALIZE(X) returns a normalized version of X where
#   the mean value of each feature is 0 and the standard deviation
#   is 1. This is often a good preprocessing step to do when
#   working with learning algorithms.

mu = colMeans(X)
X_norm <- matrix(mapply("-", X, mu), nrow=dim(X)[1], ncol=dim(X)[2], byrow=T)

sigma <- sd(X_norm)
X_norm <- matrix(mapply("/", X_norm, sigma), nrow=dim(X)[1], ncol=dim(X)[2], byrow=T)
return(list(X_norm, mu, sigma))

# ============================================================

}
