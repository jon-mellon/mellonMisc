#' Convert weighted edge list to adjacency matrix
#'
#' Builds a weighted adjacency matrix from a three-column edge list.
#' @param weighted.edge.list A data frame or matrix with `from`, `to`, and weight.
#' @return A square numeric matrix with row/column names for each actor.
#' @export
convertEdgeToAdjacencyWeighted <- function(weighted.edge.list) {
  all.actors <- unique(c(weighted.edge.list[, 1], weighted.edge.list[, 2]))
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
