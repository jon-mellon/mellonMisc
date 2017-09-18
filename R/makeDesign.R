makeDesign <- function(data, weight.var, id.var) {
  svydesign(ids = data[!is.na(data[, weight.var]), id.var], 
            data = data[!is.na(data[, weight.var]), ], 
            weights = data[!is.na(data[, weight.var]), weight.var])
}
