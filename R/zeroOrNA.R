zeroOrNA <- function(x) {
  z <- is.na(x)
  z[which(x==0)] <- TRUE
  return(z)
}