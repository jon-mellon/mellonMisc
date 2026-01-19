#' Logit marginal effects with optional clustering
#'
#' Estimates marginal effects for a logit model with robust/clustered SEs.
#' @param formula A model formula.
#' @param data A data frame.
#' @param atmean Logical; compute effects at the mean if `TRUE`.
#' @param robust Logical; use robust variance estimates.
#' @param clustervar1,clustervar2 Optional clustering variables.
#' @param start Optional starting values.
#' @param control Control list passed to `glm`.
#' @param weights Optional weights vector.
#' @return A list with `fit` and `mfx` components.
#' @export
logitmfxest.w <- function(formula, data, atmean = TRUE, robust = FALSE, clustervar1 = NULL, 
           clustervar2 = NULL, start = NULL, control = list(), weights){
    
    if(is.null(formula)){
      stop("formula is missing")
    }
    if(!is.data.frame(data)){
      stop("data arguement must contain data.frame object")
    }
    weights_vec <- NULL
    if (!missing(weights) && !is.null(weights)) {
      weights_vec <- as.numeric(weights)
      data$.mellon_weights <- weights_vec
    }
    # cluster sort part
    if(is.null(clustervar1) & !is.null(clustervar2)){
      stop("use clustervar1 arguement before clustervar2 arguement")
    }    
    if(!is.null(clustervar1)){
      if(is.null(clustervar2)){
        if(!(clustervar1 %in% names(data))){
          stop("clustervar1 not in data.frame object")
        }    
        data = data.frame(stats::model.frame(formula, data, na.action = NULL),
                          if (!is.null(weights_vec)) data$.mellon_weights,
                          data[, clustervar1])
        names(data)[dim(data)[2]] = clustervar1
        if (!is.null(weights_vec)) {
          names(data)[dim(data)[2] - 1] <- ".mellon_weights"
        }
        data = stats::na.omit(data)
      }
      if(!is.null(clustervar2)){
        if(!(clustervar1 %in% names(data))){
          stop("clustervar1 not in data.frame object")
        }    
        if(!(clustervar2 %in% names(data))){
          stop("clustervar2 not in data.frame object")
        }    
        data = data.frame(stats::model.frame(formula, data, na.action = NULL),
                          if (!is.null(weights_vec)) data$.mellon_weights,
                          data[, c(clustervar1, clustervar2)])
        names(data)[c(dim(data)[2]-1):dim(data)[2]] = c(clustervar1,clustervar2)
        if (!is.null(weights_vec)) {
          names(data)[dim(data)[2] - 2] <- ".mellon_weights"
        }
        data = stats::na.omit(data)
      }
    }
    if (is.null(weights_vec)) {
      fit = stats::glm(formula, data = data,
                       family = stats::binomial(link = "logit"),
                       x = TRUE, start = start, control = control)
    } else {
      fit = stats::glm(formula, data = data,
                       family = stats::binomial(link = "logit"),
                       x = TRUE, start = start, control = control,
                       weights = .mellon_weights)
    }
    
    # terms needed
    x1 = stats::model.matrix(fit)
    if (any(alias <- is.na(stats::coef(fit)))) {
      x1 <- x1[, !alias, drop = FALSE]
    }
    xm = as.matrix(colMeans(x1))
    be = as.matrix(stats::na.omit(stats::coef(fit)))
    k1 = length(stats::na.omit(stats::coef(fit)))
    xb = t(xm) %*% be
    fxb = ifelse(atmean == TRUE,
                 stats::plogis(xb) * (1 - stats::plogis(xb)),
                 mean(stats::plogis(x1 %*% be) * (1 - stats::plogis(x1 %*% be))))
    # get variances
    vcv = stats::vcov(fit)

    cluster_vcov <- function(fit, data, clustervar1, clustervar2 = NULL) {
      if (is.null(clustervar2)) {
        cluster <- data[, clustervar1]
      } else {
        cluster <- data[, c(clustervar1, clustervar2)]
      }
      sandwich::vcovCL(fit, cluster = cluster, type = "HC0")
    }

    if(robust){
      if(is.null(clustervar1)){
        # white correction
        vcv = sandwich::vcovHC(fit, type = "HC0")
      } else {
        if(is.null(clustervar2)){
          vcv = cluster_vcov(fit, data, clustervar1, NULL)
        } else {
          vcv = cluster_vcov(fit, data, clustervar1, clustervar2)
        }
      }
    }
    
    if(robust==FALSE & is.null(clustervar1)==FALSE){
      if(is.null(clustervar2)){
        vcv = cluster_vcov(fit, data, clustervar1, NULL)
      } else {
        vcv = cluster_vcov(fit, data, clustervar1, clustervar2)
      }
    }
    
    mfx = data.frame(mfx=fxb*be, se=NA)
    
    # get standard errors
    if(atmean){
      gr = (as.numeric(fxb)) * (diag(k1) +
              as.numeric(1 - 2 * stats::plogis(xb)) * (be %*% t(xm)))
      mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))            
    } else {
      gr = apply(x1, 1, function(x){
        as.numeric(as.numeric(stats::plogis(x %*% be) *
                                (1 - stats::plogis(x %*% be))) *
                     (diag(k1) -
                        (1 - 2 * as.numeric(stats::plogis(x %*% be))) *
                        (be %*% t(x))))
      })  
      gr = matrix(apply(gr, 1, mean), nrow = k1)
      mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))                
    }
    
    # pick out constant and remove from mfx table
    temp1 = apply(x1,2,function(x)length(table(x))==1)
    const = names(temp1[temp1==TRUE])
    mfx = mfx[row.names(mfx)!=const,]
    
    # pick out discrete change variables
    temp1 = apply(x1,2,function(x)length(table(x))==2)
    disch = names(temp1[temp1==TRUE])
    
    # calculte the disctrete change marginal effects and standard errors
    if(length(disch)!=0){
      for(i in 1:length(disch)){
        if(atmean){
          disx0 = disx1 = xm
          disx1[disch[i],] = max(x1[,disch[i]])
          disx0[disch[i],] = min(x1[,disch[i]])
          mfx[disch[i],1] = stats::plogis(t(be) %*% disx1) -
            stats::plogis(t(be) %*% disx0)
          # standard errors
          gr = stats::dlogis(t(be) %*% disx1) %*% t(disx1) -
            stats::dlogis(t(be) %*% disx0) %*% t(disx0)
          mfx[disch[i],2] = sqrt(gr %*% vcv %*% t(gr))
          
        } else {
          disx0 = disx1 = x1
          disx1[,disch[i]] = max(x1[,disch[i]])
          disx0[,disch[i]] = min(x1[,disch[i]])  
          mfx[disch[i],1] = mean(stats::plogis(disx1 %*% be) -
                                   stats::plogis(disx0 %*% be))
          # standard errors
          gr = as.numeric(stats::dlogis(disx1 %*% be)) * disx1 -
            as.numeric(stats::dlogis(disx0 %*% be)) * disx0
          avegr = as.matrix(colMeans(gr))
          mfx[disch[i],2] = sqrt(t(avegr) %*% vcv %*% avegr)
        }
      }
    } 
    mfx$discretechgvar = ifelse(rownames(mfx) %in% disch, 1, 0)
    output = list(fit=fit, mfx=mfx)
    return(output)
  }

logitmfx.w <- function (formula, data, atmean = TRUE, robust = FALSE, clustervar1 = NULL, 
          clustervar2 = NULL, start = NULL, control = list(), weights) {
  res = logitmfxest.w(formula, data, atmean, robust, clustervar1, 
                    clustervar2, start, control, weights)
  est = NULL
  est$mfxest = cbind(dFdx = res$mfx$mfx, StdErr = res$mfx$se, 
                     z.value = res$mfx$mfx/res$mfx$se,
                     p.value = 2 * stats::pt(-abs(res$mfx$mfx/res$mfx$se),
                                             df = Inf))
  colnames(est$mfxest) = c("dF/dx", "Std. Err.", "z", "P>|z|")
  rownames(est$mfxest) = rownames(res$mfx)
  est$fit = res$fit
  est$dcvar = rownames(res$mfx[res$mfx$discretechgvar == 1, 
                               ])
  est$call = match.call()
  class(est) = "logitmfx"
  est
}
