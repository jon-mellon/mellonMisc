factorInteractions <- function(x, y) {
	if(!is.factor(x)) {
		stop("X is not a factor")
	}
	if(!is.factor(y)) {
		stop("Y is not a factor")
	}
	levels <- unlist(lapply(levels(x), paste, levels(y)))
	z <- paste(x, y)
	z[which(is.na(x))] <- NA
	z[which(is.na(y))] <- NA
	z <- factor(z, labels = levels)#
	return(z)
}
