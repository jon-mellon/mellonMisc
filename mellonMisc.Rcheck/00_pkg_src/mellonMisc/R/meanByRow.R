#' Row-wise mean
#'
#' Compute row-wise means across vectors or columns.
#' @param ... Numeric vectors or a matrix-like object to bind by column.
#' @param na.rm Logical; remove missing values.
#' @return Numeric vector of row means.
#' @export
meanByRow <- function(..., na.rm) {
  data <- as.matrix(cbind(...))
  out <- matrixStats::rowMeans2(data, na.rm = na.rm)
  out[out==-Inf] <- NA
  return(out)
}
