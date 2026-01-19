#' Standardize place-name formatting
#'
#' Cleans and standardizes geographic names with consistent capitalization.
#' @param x A character vector of names.
#' @return A character vector of standardized names.
#' @export
formatStandard <- function(x) {
	x <- gsub(" and ", " & ", x)
	x <- gsub(" And ", " & ", x)
	x <- gsub("[[:punct:]]", "", x)
	x <- Hmisc::capitalize(x)
	x <- gsub(" +", " ", x)
	x <- gsub("^City of ", "", x)
	x <- gsub(" Borough Council", "", x)
	x <- gsub(" District Council", "", x)
	x <- gsub(" County Council", "", x)
	x <- gsub(" County", "", x)
	x <- gsub(" City Council", "", x)
	x <- gsub(" Council", "", x)
	x <- gsub("^ | $", "", x)
	x <- tolower(x)
	x <- strsplit(x, " ")	
	x <- sapply(x , function(x) Hmisc::capitalize(x))
	x <- sapply(x, paste, collapse = " ") 
	return(x)
}
