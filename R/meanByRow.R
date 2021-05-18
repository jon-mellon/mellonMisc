meanByRow <- function(..., na.rm ) {
  data <- as.matrix(cbind(...))
  out <- matrixStats::rowMeans(data, na.rm = na.rm)
  out[out==-Inf] <- NA
  return(out)
}