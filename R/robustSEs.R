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

# summary(sig.predictors.domestic.non.us)
# robustSEs(sig.predictors.domestic.non.us, cluster = "country_target")
# robustSEs(sig.predictors.domestic.us)
# robustSEs(sig.predictors.foreign.non.us, cluster = "country_target")
# robustSEs(sig.predictors.foreign.us)
# 
# summary(sig.predictors.domestic.us)
# nrow(sig.predictors.domestic.us$model)
# 
# 
# summary(sig.predictors.foreign.non.us)
# 
# coeftest(sig.predictors.domestic, vcov=function(x) vcovHC(x, cluster="country_target", type="HC1"))
# coeftest(sig.predictors.foreign, vcov=function(x) vcovHAC(x, cluster="country_target", type="HC1"))
# install.packages("clusterSEs")
# args(cluster.im.glm)
# petition.comb$country_target
# 
# cluster.im.glm(sig.predictors.domestic, dat = petition.comb,
#                cluster = ~ country_target)
