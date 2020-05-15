#' @export edgeListToAdjacency
edgeListToAdjacency <- function(weighted.edge.list) {
  weighted.edge.list[, 1] <- as.character(weighted.edge.list[, 1])
  weighted.edge.list[, 2] <- as.character(weighted.edge.list[, 2])
	all.actors <- unique(c(weighted.edge.list[, 1], weighted.edge.list[, 2]))
	actor <-all.actors [2]
	fillInAdjacencyRow <- function(actor, all.actors, weighted.edge.list) {
		actor.edges <- weighted.edge.list[weighted.edge.list[, 1] == actor, 2:3]
		row <- rep(0, length(all.actors))
		row[match(actor.edges[, 1], all.actors)] <- actor.edges[, 2]
		return(row)
	}
	
	adjacency <- sapply(all.actors, FUN = fillInAdjacencyRow, 
											all.actors = all.actors, 
											weighted.edge.list = weighted.edge.list)
	adjacency <- t(adjacency)
	rownames(adjacency) <- all.actors
	colnames(adjacency) <- all.actors
	return(adjacency)
}