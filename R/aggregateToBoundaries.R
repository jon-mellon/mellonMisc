#' @export aggregateToBoundaries
aggregateToBoundaries <- function(df, identifier.x, weight.x, lookup) {
  lookup <- lookup[which(lookup[, 1] %in% unlist(df[, identifier.x])), ]
  colnames(lookup) <- c("identifier.x", "identifier.y")
  lookup <- dtf(lookup)
  df <- safemerge(df, lookup, by.x = identifier.x, by.y = "identifier.x", type= "1:m")
  
  df.multiple <- split( df[, !colnames(df)%in% c(identifier.x, "identifier.y")],
                        f = list(df$identifier.y))
  output.data <- t(sapply(X = df.multiple, 
                          function(x) apply(as.matrix(x[, 1:ncol(x)]), 2, weighted.mean, 
                                            na.rm = T,
                                            w = unlist(x[, weight.x]))))
  output.data <- dtf(output.data)
  
  output.data$identifier.y <- rownames(output.data)
  output.data[, weight.x] <- NULL
  return(output.data)
}