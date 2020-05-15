#' @export convertOccs
convertOccs <- function(occs, from, to ) {
  data("occ.crosswalks")
  occ.edges <- do.call(rbind, strsplit(names(occ.crosswalks), "-"))
  library(igraph)
  occ.graph <- graph.edgelist(occ.edges, directed = T)
  
  # plot(occ.graph)
  relevant.crosswalks <- all_shortest_paths(occ.graph, from = from, to = to)$res[[1]]
  
  cw.sets <- c()
  for(ii in 1:(length(relevant.crosswalks) - 1)) {
    cw.sets[ii] <- paste(names(relevant.crosswalks[ii:(ii+1)]), collapse = "-")
  }
  
  for(jj in cw.sets) {
    occs <- occ.crosswalks[[jj]][, 2][match(occs, occ.crosswalks[[jj]][, 1])]
  }
  return(occs)
}

