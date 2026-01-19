#' Flag zero or missing values
#'
#' Identify elements that are either zero or `NA`.
#' @param x A numeric vector.
#' @return A logical vector where `TRUE` indicates `0` or `NA`.
#' @export
zeroOrNA <- function(x) {
  z <- is.na(x)
  z[which(x==0)] <- TRUE
  return(z)
}
