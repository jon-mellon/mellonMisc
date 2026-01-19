#' Dplyr select shortcut
#'
#' Convenience wrapper around `dplyr::select()`.
#' @param ... Arguments passed to `dplyr::select()`.
#' @return A subset of columns from a data frame/tibble.
#' @export
slct <- function(...) {
  dplyr::select(...)
} 
