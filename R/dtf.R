#' @export dtf
dtf <- function(..., StAsFa= FALSE) {
  data.frame(..., stringsAsFactors = StAsFa)
}