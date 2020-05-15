#' @export assignToBoundaries
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
  totals <- colSums(trans.mat * thing.to.assign, na.rm = T)
  
  
  if(all(round(rowSums(trans.mat * thing.to.assign), 3)!=round(thing.to.assign, 3))) {
    stop("Total do not reconcile")
  }
  return(totals)
}
