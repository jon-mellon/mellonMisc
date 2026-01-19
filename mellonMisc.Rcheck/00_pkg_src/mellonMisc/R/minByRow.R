#' Row-wise minimum
#'
#' Compute row-wise minima across vectors or columns.
#' @param ... Numeric vectors or a matrix-like object to bind by column.
#' @param na.rm Logical; remove missing values.
#' @return Numeric vector of row minima.
#' @export
minByRow <- function(..., na.rm) {
  data <- as.matrix(cbind(...))
  out <- matrixStats::rowMins(data, na.rm = na.rm)
  out[out==Inf] <- NA
  return(out)
}
