getMiddleInitial <- function(name) {
  name <- strsplit(name, " ")
  middles <- sapply(name, function(x) x[2])
  middles[sapply(name, length)<3] <- NA
  middles <- substr(middles, 1, 1)
  middles <- tolower(middles)
  return(middles)
}
