sumByRow <- function(..., na.rm ) {
  data <- as.matrix(cbind(...))
  out <- rowSums(data, na.rm = na.rm)
  out[out==Inf] <- NA
  return(out)
}
