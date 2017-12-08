tToV <- function(x) {
  if(length(dim(x))!=1) {
    stop("Dimensionality>1")
  }
  return(as.matrix(x)[, 1])
}