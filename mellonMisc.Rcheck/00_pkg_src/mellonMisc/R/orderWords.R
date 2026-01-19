#' Sort words within strings
#'
#' Alphabetically sorts words within each string.
#' @param x A character vector.
#' @return A character vector with sorted words.
#' @export
orderWords <- function(x) {
	x <- strsplit(x, " ")
	x <- lapply(x, sort)
	x <- sapply(x, paste, collapse = " ")
	return(x)
}
