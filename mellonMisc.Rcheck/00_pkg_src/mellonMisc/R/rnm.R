#' Dplyr rename shortcut
#'
#' Convenience wrapper around `dplyr::rename()`.
#' @param ... Arguments passed to `dplyr::rename()`.
#' @return A renamed data frame/tibble.
#' @export
rnm <- function(...) {
  dplyr::rename(...)
}
