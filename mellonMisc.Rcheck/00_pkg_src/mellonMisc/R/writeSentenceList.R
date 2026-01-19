#' Format a list as a sentence
#'
#' Joins a character vector with commas and an optional Oxford comma.
#' @param x A character vector.
#' @param oxcomma Logical; include an Oxford comma if `TRUE`.
#' @return A single character string.
#' @export
writeSentenceList <- function(x, oxcomma = TRUE) {
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
