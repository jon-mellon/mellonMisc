#' Row-wise maximum
#'
#' Compute row-wise maxima across vectors or columns.
#' @param ... Numeric vectors or a matrix-like object to bind by column.
#' @param na.rm Logical; remove missing values.
#' @return Numeric vector of row maxima.
#' @export
maxByRow <- function(..., na.rm) {
  data <- as.matrix(cbind(...))
  out <- matrixStats::rowMaxs(data, na.rm = na.rm)
  out[out==-Inf] <- NA
  return(out)
}
