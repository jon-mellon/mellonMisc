#' @export formatStandard
formatStandard <- function(x) {
	x <- gsub(" and ", " & ", x)
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
	return(x)
}