#' Format a UK postcode
#'
#' Splits a UK postcode into area and full formatted postcode.
#' @param postcode A character string of a postcode.
#' @return A character vector with `area` and `postcode`.
#' @export
getPostcode <- function(postcode) {
	# This function correctly formats a UK postcode
	end.code <- substr(postcode, start = nchar(postcode) - 2, stop = nchar(postcode))
	postcode.area <- gsub(end.code, "", postcode)
	postcode.area <- gsub("[[:space:]]", "", postcode.area)
	return(c(area = postcode.area, postcode = paste(postcode.area, end.code)))
}
