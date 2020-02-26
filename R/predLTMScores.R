predGRMScores <- function(data, mod) {
  vals <- ltm::factor.scores(mod)
  colnames(vals)
  vals <- vals$score.dat
  vars <- vals[, !colnames(vals) %in% c("Obs", "Exp", "z1", "se.z1")]
  data <- data[, colnames(vars)]
  # data <- data[rowSums(is.na(data))<ncol(vars), ]
  index <- apply(data, 1, paste0, collapse = "|")
  index.vars <- apply(vars, 1, paste0, collapse = "|")
  output <- dtf(z = vals$z1[match(index, index.vars)], 
                se = vals$se.z1[match(index, index.vars)])
  output[rowSums(is.na(data))==ncol(vars), ] <- NA
  return(output)
}
