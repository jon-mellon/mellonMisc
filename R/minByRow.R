minByRow <- function(..., na.rm ) {
  data <- as.matrix(cbind(...))
  out <- matrixStats::rowMins(data, na.rm = na.rm)
  out[out==Inf] <- NA
  return(out)
}
