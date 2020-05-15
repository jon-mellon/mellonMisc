#' @export predict.clogit
predict.clogit <- function(object, newdata = NULL, 
                           type = "response",
                           formula, strata.var) {
  if(type!="response") {
    stop("type=response is the only outcome type currently supported.")
  }
  if(!is.null(newdata$new_strata_variable)) {
    stop("Cannot have existing variable named new_strata_variable. Call it literally anything else.")
  }
  newdata$new_strata_variable <- newdata[, strata.var]
  mod.mat <- model.matrix(formula, data = newdata)
  
  mf <- model.frame(update(formula, new = ~.+ new_strata_variable), 
                    data = newdata)
  
  exponents <- exp(mod.mat %*% coef(object))
  getProbs <- function(x) {
    ids <- sapply(x, function(y) y$id)
    exps <- sapply(x, function(y) y$exp)
    dtf(id = ids, probs = exps / sum(exps))
  }
  exponents <- split(data.frame(id = 1:nrow(mf), 
                                exp = exponents), 1:nrow(mf))
  
  all.probs <- tapply(exponents, mf$new_strata_variable, getProbs)
  all.probs <- do.call(rbind, all.probs)
  all.probs <- all.probs[order(all.probs$id), ]
  return(all.probs$probs)
}