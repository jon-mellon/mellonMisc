#' Plot a discrete variable
#'
#' Creates a percentage bar plot for a discrete variable.
#' @param var Column name in `data` to plot.
#' @param data A data frame containing the variable.
#' @param delete.values Values to exclude from the plot.
#' @return A ggplot object.
#' @export
plotDiscrete <- function(var, data, delete.values = c()) {
	x <- data[, var]
	x <- x[!is.na(x)]
	x <- x[!x %in% delete.values]
	levels(x)	<- gsub("_", " ", levels(x))
	var <- gsub("_|\\.", " ", var)
	var <- gsub("incomeb", "income", var)
	var <- Hmisc::capitalize(var)
	
	discrete <- data.frame(table(x) / sum(!is.na(x)) * 100)
	plot <- ggplot2::ggplot(discrete, ggplot2::aes(y = Freq, x = x)) +
		ggplot2::geom_col() +
		ggplot2::ylab("Percentage of responses") + ggplot2::xlab("") +
		ggplot2::ggtitle(var) +
		ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, hjust = 0)) +
		ggplot2::ylim(0, 100)
	return(plot)
}
