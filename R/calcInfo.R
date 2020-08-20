calcInfo <- function(mod, data) {
  grm.scores <- predGRMScores(mod = mod, data)
  var.E <- mean(grm.scores$se ^ 2)
  var.theta <- var(grm.scores$z)
  reliability <- 1 - (var.E / (var.E + var.theta))
  return(reliability)
}