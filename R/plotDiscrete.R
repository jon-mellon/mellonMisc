plotDiscrete <- function(var, data, delete.values = c()) {
	require(ggplot2)
	require(Hmisc)
	x <- data[, var]
	x <- x[!is.na(x)]
	x <- x[!x %in% delete.values]
	levels(x)	<- gsub("_", " ", levels(x))
	var <- gsub("_|\\.", " ", var)
	var <- gsub("incomeb", "income", var)
	var <- capitalize(var)
	
	discrete <- data.frame(table(x) / sum(!is.na(x)) * 100)
	plot <- ggplot(aes(y = Freq, x = x), data = discrete) + geom_bar(aes(stat = "identity"))+
		ylab("Percentage of responses") + xlab("") + ggtitle(var) +
		opts(axis.text.x=theme_text(angle=-90)) + ylim(0,100)
	return(plot)
}