allDup <- function(x) {
  duplicated(x) | duplicated(x, fromLast = TRUE)
}