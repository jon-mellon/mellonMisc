#' Convert Excel serial dates
#'
#' Converts Windows Excel serial dates to `Date`.
#' @param x Numeric vector of Excel serial dates.
#' @return A `Date` vector.
#' @export
excelWinDate <- function(x) {
  return(as.Date(x, origin =  "1899-12-30"))
}
