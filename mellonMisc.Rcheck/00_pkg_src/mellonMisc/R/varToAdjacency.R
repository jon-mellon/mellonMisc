#' Convert a variable to an adjacency matrix
#'
#' Builds a diagonal adjacency matrix from a variable over nodes.
#' @param df A data frame containing the variable.
#' @param var Column name of the variable to place on the diagonal.
#' @param cvar Column name identifying nodes.
#' @param relevant.countries Vector of nodes to include.
#' @param col Logical; transpose the matrix if `TRUE`.
#' @return A square numeric matrix.
#' @export
varToAdjacency <- function(df, var, cvar, relevant.countries, col = TRUE) {
  df <- df[, c(cvar, var)]
  relevant.countries <- sort(relevant.countries)
  start.point <- data.frame(country = relevant.countries, stringsAsFactors = FALSE)
  df <- merge(x = start.point, y = df, by.x = "country", by.y = cvar, 
              all.x = TRUE, all.y = FALSE)
  
  adjacency <- as.matrix( df[, rep(var, length(relevant.countries))] )
  rownames(adjacency) <- relevant.countries
  colnames(adjacency) <- relevant.countries
  if(col) {
    return(t(adjacency))
  } else {
    return(adjacency)
  }
}
