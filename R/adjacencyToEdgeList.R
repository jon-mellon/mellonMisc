#' @export adjacencyToEdgeList
adjacencyToEdgeList <- function(adjacency) {
	edge.list <- t(combn(c(colnames(adjacency), rownames(adjacency)), 2))
	edge.list <- edge.list[!duplicated(edge.list), ]
	colnames(edge.list) <- c("source", "target")
	edge.list <- data.frame(edge.list, stringsAsFactors = FALSE)
	edge.list$value <- 0
	
	for(ii in 1:nrow(edge.list)) {
		edge.list[ii, "value"] <- adjacency[edge.list[ii, "source"], edge.list[ii, "target"]]
	}
	edge.list <- edge.list[edge.list$value!=0, ]
	return(edge.list)
}






