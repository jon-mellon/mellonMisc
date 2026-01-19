#' Convert a variable to an adjacency matrix
#'
#' Builds a diagonal adjacency matrix from a variable over nodes.
#' @param df A data frame containing the variable.
#' @param var Column name of the variable to place on the diagonal.
#' @param cvar Column name identifying nodes.
#' @param relevant.nodes Vector of nodes to include.
#' @param col Logical; transpose the matrix if `TRUE`.
#' @return A square numeric matrix.
#' @export
varToAdj <- function(df, var, cvar, relevant.nodes, col = TRUE) {
  df <- df[, c(cvar, var)]
  relevant.nodes <- sort(relevant.nodes)
  start.point <- data.frame(node = relevant.nodes, stringsAsFactors = FALSE)
  df <- merge(x = start.point, y = df, by.x = "node", by.y = cvar,
              all.x = TRUE, all.y = FALSE)

  adjacency <- as.matrix(df[, rep(var, length(relevant.nodes))])
  rownames(adjacency) <- relevant.nodes
  colnames(adjacency) <- relevant.nodes
  if(col) {
    return(t(adjacency))
  } else {
    return(adjacency)
  }
}
