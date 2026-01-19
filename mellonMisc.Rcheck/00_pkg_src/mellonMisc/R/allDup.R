#' Identify all duplicates
#'
#' Returns `TRUE` for values that appear more than once.
#' @param x A vector to check for duplicates.
#' @return A logical vector marking all duplicates.
#' @export
allDup <- function(x) {
  duplicated(x) | duplicated(x, fromLast = TRUE)
}
