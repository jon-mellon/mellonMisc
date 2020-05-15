#' @export robustSEs
robustSEs <- function(fit, cluster = NULL) {
  library(effects)
  library(sandwich)
  library(lmtest)
  library(clusterSEs)
  
  if (is.null(cluster)) {
    bread <-vcov(fit)
    est.fun <- estfun(fit)
    meat <- t(est.fun) %*% est.fun
    sandwich <- bread %*% meat %*% bread
    return(coeftest(fit, sandwich))
  }
  require(sandwich)

  fit$data <- fit$data[rownames(fit$data ) %in%  rownames(fit$model), ]
  fc <- fit$data[, cluster]
  m <- length(unique(fc))
  k <- length(coef(fit))
  
  u <- estfun(fit)
  u.clust <- matrix(NA, nrow=m, ncol=k)
  for(j in 1:k){
    u.clust[,j] <- tapply(u[,j], fc, sum)
  }
  cl.vcov <- vcov(fit) %*% ((m / (m-1)) * t(u.clust) %*% (u.clust))  %*% + vcov(fit)
  return(coeftest(fit, cl.vcov))
}
