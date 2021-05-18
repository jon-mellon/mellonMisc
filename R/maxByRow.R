maxByRow <- function(..., na.rm ) {
  data <- as.matrix(cbind(...))
  out <- matrixStats::rowMaxs(data, na.rm = na.rm)
  out[out==-Inf] <- NA
  return(out)
}