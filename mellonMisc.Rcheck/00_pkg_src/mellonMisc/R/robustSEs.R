#' Robust standard errors
#'
#' Computes robust or clustered standard errors for a fitted model.
#' @param fit A fitted model object.
#' @param cluster Optional clustering variable name.
#' @return A `lmtest::coeftest` object.
#' @export
robustSEs <- function(fit, cluster = NULL) {
  if (is.null(cluster)) {
    bread <- stats::vcov(fit)
    est.fun <- sandwich::estfun(fit)
    meat <- t(est.fun) %*% est.fun
    sandwich <- bread %*% meat %*% bread
    return(lmtest::coeftest(fit, sandwich))
  }

  fit$data <- fit$data[rownames(fit$data ) %in%  rownames(fit$model), ]
  fc <- fit$data[, cluster]
  m <- length(unique(fc))
  k <- length(coef(fit))
  
  u <- sandwich::estfun(fit)
  u.clust <- matrix(NA, nrow=m, ncol=k)
  for(j in 1:k){
    u.clust[,j] <- tapply(u[,j], fc, sum)
  }
  cl.vcov <- stats::vcov(fit) %*% ((m / (m - 1)) * t(u.clust) %*% (u.clust)) %*%
    stats::vcov(fit)
  return(lmtest::coeftest(fit, cl.vcov))
}
