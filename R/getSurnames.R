getSurnames <- function(x) {
	x <- gsub(" +", " ", x)
	x <- strsplit(x, " ")
	surnames <- sapply(x, function(x) x[length(x)])
	return(surnames)
}