#' @export cooccurenceToAdjacency
cooccurenceToAdjacency <- function (term.document.matrix, sum.weights = FALSE) {
  if(sum.weights) {
    countCooccurence <- function(term1, term2, term.doc.matrix) {
      present <- term.doc.matrix[, term1]!=0 & term.doc.matrix[, term2]!=0
      sums <- term.doc.matrix[, term1] + term.doc.matrix[, term2]
      return(sum(sums[present]))
    }
    
  } else {
    countCooccurence <- function(term1, term2, term.doc.matrix) {
      return(sum(term.doc.matrix[, term1] & term.doc.matrix[, 
                                                            term2], na.rm = T))
    }
  }
  fillAdjacencyRow <- function(current.term, all.terms) {
    return(apply(X = as.matrix(all.terms), MARGIN = 1, FUN = countCooccurence, 
                 term1 = current.term, term.doc.matrix = term.document.matrix))
  }
  all.terms <- colnames(term.document.matrix)
  adjacency <- apply(as.matrix(all.terms), MARGIN = 1, FUN = fillAdjacencyRow, 
                     all.terms = all.terms)
  colnames(adjacency) <- all.terms
  rownames(adjacency) <- all.terms
  return(adjacency)
}