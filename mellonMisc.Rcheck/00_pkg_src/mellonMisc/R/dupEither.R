#' Identify duplicates from either direction
#'
#' Returns `TRUE` for elements duplicated from the start or end.
#' @param x A vector to check for duplicates.
#' @return A logical vector marking duplicates.
#' @export
dupEither <- function(x) {
  duplicated(x) | duplicated(x, fromLast = TRUE)
}
