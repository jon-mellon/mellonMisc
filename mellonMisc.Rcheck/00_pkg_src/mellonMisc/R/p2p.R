#' Proportion to percentage
#'
#' Converts proportions to percentages.
#' @param x Numeric vector of proportions.
#' @param round Number of decimal places to round to.
#' @return Numeric vector of percentages.
#' @export
p2p <- function(x, round = 1) {
  # proportion to percentage
  round(x * 100, round)
}
