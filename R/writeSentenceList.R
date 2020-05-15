#' @export writeSentenceList
writeSentenceList <- function(x, oxcomma = T) {
  if(length(x) >2) {
    last.item <- x[length(x)]
    x <- x[-length(x)]
    if(oxcomma) {
      x <- paste0(paste(x, collapse = ", "), ", and ", last.item)  
    } else {
      x <- paste0(paste(x, collapse = ", "), " and ", last.item)  
    }
    return(x)
  } 
  if(length(x)==2) {
    x <- paste(x, collapse = " and ")
  }
  return(x)
}