#' Combine factor levels
#'
#' Creates an interaction factor from two factors, preserving labels.
#' @param x,y Factor vectors of equal length.
#' @return A factor with combined levels.
#' @export
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
