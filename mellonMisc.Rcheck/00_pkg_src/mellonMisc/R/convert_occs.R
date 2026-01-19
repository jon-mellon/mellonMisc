#' Convert occupation codes
#'
#' Converts occupation codes between classifications using shortest paths.
#' @param occs A vector of occupation codes to convert.
#' @param from The source classification label.
#' @param to The target classification label.
#' @return A vector of converted occupation codes.
#' @export
convertOccs <- function(occs, from, to ) {
  data("occ.crosswalks")
  occ.edges <- do.call(rbind, strsplit(names(occ.crosswalks), "-"))
  occ.graph <- igraph::graph.edgelist(occ.edges, directed = TRUE)
  
  # plot(occ.graph)
  relevant.crosswalks <- igraph::all_shortest_paths(occ.graph, from = from, to = to)$res[[1]]
  
  cw.sets <- c()
  for(ii in 1:(length(relevant.crosswalks) - 1)) {
    cw.sets[ii] <- paste(names(relevant.crosswalks[ii:(ii+1)]), collapse = "-")
  }
  
  for(jj in cw.sets) {
    occs <- occ.crosswalks[[jj]][, 2][match(occs, occ.crosswalks[[jj]][, 1])]
  }
  return(occs)
}

