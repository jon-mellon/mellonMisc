predictAt <- function(mod, at ) {
	vars <- names(at)
	at <- expand.grid(at)
	at$prob <- NA
	at$se <- NA
	
  for(ii in 1:nrow(at)) {
    data <- model.frame(mod)
    if(any(!vars %in% names(data))) {
      stop("Variable not found in data")
    }
    for(jj in vars) {
      data[, jj] <- at[ii, jj]
    }
    pred.prob <- predict(mod, newdata = data, type = "response", se.fit = T)
    
    at[ii, "prob"] <- wtd.mean(pred.prob$fit, weights = data$`(weights)`, na.rm = T)
    at[ii, "se"]   <- wtd.mean(pred.prob$se.fit, weights = data$`(weights)`, na.rm = T)
  }
  at$lci <- at$prob - 1.96 * at$se
  at$uci <- at$prob + 1.96 * at$se
  return(at)
}
