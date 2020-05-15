#' @export dupEither
dupEither <- function(x) {
  duplicated(x) | duplicated(x, fromLast = T)
}
