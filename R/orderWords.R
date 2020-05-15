#' @export orderWords
orderWords <- function(x) {
	x <- strsplit(x, " ")
	x <- lapply(x, sort)
	x <- sapply(x, paste, collapse = " ")
	return(x)
}