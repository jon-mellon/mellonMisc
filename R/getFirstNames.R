getFirstNames <- function(x) {
	
	x <- gsub(" +", " ", x)
	x <- gsub("\\.", "", x)
	x <- strsplit(x, " ")
	
	first.names <- sapply(x, function(x) x[1])
	first.names <- toupper(first.names)
	removals <- c("DR", "THE", "MR", "MRS",
								"MS")
	first.names[nchar(first.names) == 1] <- NA
	first.names[first.names %in% removals] <- 
		sapply(x, function(x) x[2])[first.names %in% removals]		
	return(first.names)	
}
