makeDesign <- function(data, weight.var, id.var) {
  svydesign(ids = data[!is.na(data[, weight.var]) & !is.na(data[, id.var]), id.var], 
            data = data[!is.na(data[, weight.var]) & !is.na(data[, id.var]), ], 
            weights = data[!is.na(data[, weight.var]) & !is.na(data[, id.var]), weight.var])
}
