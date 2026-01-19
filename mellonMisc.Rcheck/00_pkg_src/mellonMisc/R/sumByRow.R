#' Row-wise sum
#'
#' Compute row-wise sums across vectors or columns.
#' @param ... Numeric vectors or a matrix-like object to bind by column.
#' @param na.rm Logical; remove missing values.
#' @return Numeric vector of row sums.
#' @export
sumByRow <- function(..., na.rm) {
  data <- as.matrix(cbind(...))
  out <- rowSums(data, na.rm = na.rm)
  out[out==Inf] <- NA
  return(out)
}
