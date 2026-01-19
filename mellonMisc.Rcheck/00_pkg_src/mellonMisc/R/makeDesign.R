#' Build a survey design object
#'
#' Convenience wrapper to create a `survey::svydesign` object.
#' @param data A data frame with survey data.
#' @param weight.var Column name for survey weights.
#' @param id.var Column name for cluster/PSU ids.
#' @return A `survey.design` object.
#' @export
makeDesign <- function(data, weight.var, id.var) {
  survey::svydesign(ids = data[!is.na(data[, weight.var]) & !is.na(data[, id.var]), id.var], 
            data = data[!is.na(data[, weight.var]) & !is.na(data[, id.var]), ], 
            weights = data[!is.na(data[, weight.var]) & !is.na(data[, id.var]), weight.var])
}
