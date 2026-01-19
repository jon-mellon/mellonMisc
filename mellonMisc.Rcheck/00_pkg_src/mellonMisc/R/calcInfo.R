#' Estimate reliability from GRM scores
#'
#' Computes an approximate reliability from graded response model scores.
#' @param mod A fitted IRT model.
#' @param data A data frame used for scoring.
#' @return A single numeric reliability estimate.
#' @export
calcInfo <- function(mod, data) {
  grm.scores <- predGRMScores(mod = mod, data)
  var.E <- mean(grm.scores$se ^ 2)
  var.theta <- var(grm.scores$z)
  reliability <- 1 - (var.E / (var.E + var.theta))
  return(reliability)
}
