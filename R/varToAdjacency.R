#' @export varToAdjacency
varToAdjacency <- function(df, var, cvar, relevant.countries, col = T) {
  df <- df[, c(cvar, var)]
  relevant.countries <- sort(relevant.countries)
  start.point <- data.frame(country = relevant.countries, stringsAsFactors = F)
  df <- merge(x = start.point, y = df, by.x = "country", by.y = cvar, 
              all.x = T, all.y = F)
  
  adjacency <- as.matrix( df[, rep(var, length(relevant.countries))] )
  rownames(adjacency) <- relevant.countries
  colnames(adjacency) <- relevant.countries
  if(col) {
    return(t(adjacency))
  } else {
    return(adjacency)
  }
}
