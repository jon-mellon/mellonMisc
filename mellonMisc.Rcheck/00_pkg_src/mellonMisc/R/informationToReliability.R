#' Convert information to reliability
#'
#' Converts information values to reliability using a standard transformation.
#' @param I A numeric vector of information values.
#' @return A numeric vector of reliability values.
#' @export
informationToReliability <- function(I) {
  se = 1/sqrt(I)
  1 - (se^2 / (1 + se^2))
}
