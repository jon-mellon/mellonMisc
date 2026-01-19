#' Data frame shortcut
#'
#' Convenience wrapper around `data.frame()` with `stringsAsFactors` control.
#' @param ... Values passed to `data.frame()`.
#' @param StAsFa Logical; set `stringsAsFactors`.
#' @return A data frame.
#' @export
dtf <- function(..., StAsFa = FALSE) {
  data.frame(..., stringsAsFactors = StAsFa)
}
