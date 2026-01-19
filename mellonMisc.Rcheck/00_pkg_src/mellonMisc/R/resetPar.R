#' Reset graphics parameters
#'
#' Restores graphics parameters to device defaults.
#' @return The previous graphics parameters (invisibly).
#' @export
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
  par(op)
}
