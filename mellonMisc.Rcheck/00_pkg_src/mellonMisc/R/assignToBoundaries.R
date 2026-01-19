#' Allocate values using a transition matrix
#'
#' Uses a weighted transition matrix to allocate values from origins to targets.
#' @param trans.mat A matrix of weights mapping origins (rows) to targets (cols).
#' @param thing.to.assign Numeric vector of values to allocate, aligned to `labels`.
#' @param labels Origin labels matching the rows of `trans.mat`.
#' @return A numeric vector of totals for each target column.
#' @export
assignToBoundaries <- function(trans.mat, thing.to.assign, labels) {
  # this function expects to receive a transition matrix with weights 
  # that determine what proportion of each row gets assigned to each column 
  if(!all(labels %in% rownames(trans.mat))) {
    warning(paste0(sum(!labels %in% rownames(trans.mat)), 
                   " origins do not appear in the transition matrix"))
  }
  trans.mat[is.na(trans.mat)] <- 0
  trans.mat <- prop.table(trans.mat, 1)
  
  trans.mat <- trans.mat[match(labels, rownames(trans.mat)), ]
  totals <- colSums(trans.mat * thing.to.assign, na.rm = TRUE)
  
  
  if(all(round(rowSums(trans.mat * thing.to.assign), 3)!=round(thing.to.assign, 3))) {
    stop("Total do not reconcile")
  }
  return(totals)
}
