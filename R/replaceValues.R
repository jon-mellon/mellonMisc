#' @export replaceValues
replaceValues <- function(text, subs) {
  text.list <- strsplit(text, split = " ")
  
  replace <- function(part) {
    tmp.subs <- subs[subs[, 1] %in% part, , drop = FALSE]
    tmp.subs <- unique(tmp.subs)  
    if(nrow(tmp.subs)!= 0) {
      for(i in 1:nrow(tmp.subs)) {
        part[part==tmp.subs[i, 1]] <- tmp.subs[i, 2]
      }
    }
    return(part)
  }
  text.list <- lapply(text.list, replace)
  text <- sapply(text.list, paste, collapse = " ")
  return(text)
}