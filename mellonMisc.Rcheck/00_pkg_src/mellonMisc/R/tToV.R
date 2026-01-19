#' Table to vector
#'
#' Converts a 1D table to a vector.
#' @param x A one-dimensional table or matrix.
#' @return A vector of values.
#' @export
tToV <- function(x) {
  if(length(dim(x))!=1) {
    stop("Dimensionality>1")
  }
  return(as.matrix(x)[, 1])
}
