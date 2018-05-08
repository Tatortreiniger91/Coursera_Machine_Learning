displayData <- function(X, example_width = round(sqrt(dim(X)[2]))){
#DISPLAYDATA Display 2D data in a nice grid
#   [h, display_array] = DISPLAYDATA(X, example_width) displays 2D data
#   stored in X in a nice grid. It returns the figure handle h and the 
#   displayed array if requested.


# Compute rows, cols
m <- dim(X)[1]
n <- dim(X)[2]
example_height <- n/example_width

# Compute number of items to display
display_rows <- floor(sqrt(m))
display_cols <- ceiling(m/display_rows)

# Between images padding
pad <- 1

# Setup blank display
display_array <- matrix(0, nrow = (pad + display_rows * (example_height + pad)), ncol= (pad + display_cols * (example_width + pad)))

# Copy each example into a patch on the display array
curr_ex <- 1
for(j in 1:display_rows) {
  for(i in 1:display_cols) {
    if(curr_ex > m)
      break
    max_val <- max(abs(X[curr_ex,]))
    display_array[pad + (j - 1) * (example_height + pad) + (1:example_height),
                  pad + (i - 1) * (example_width + pad) + (1:example_width)] <-
      matrix(X[curr_ex,], example_height, example_width) / max_val
    curr_ex <- curr_ex + 1
  }
  if(curr_ex > m)
    break
}

# image() draws by row from from bottom to top, but R indexes the matrix by column and from top to down
display_array <- display_array <- apply(display_array, 2, rev)
display_array <- t(display_array)
image(display_array, col = gray.colors(200), axes = FALSE)

}
