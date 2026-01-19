#' Basic text cleaning
#'
#' Lowercases text and removes punctuation, digits, and extra whitespace.
#' @param text A character vector to clean.
#' @return A character vector with normalized whitespace.
#' @export
cleanText <- function(text) {
  text <- tolower(text)
  #   text[!text %in% c("__na__", "", " na ")] <- NA
  text <- gsub("[[:punct:][:digit:]]", " ", text)
  text <- gsub("\\\r", " ", text)
  text <- gsub("\\\n", " ", text)
  text <- gsub("[[:space:]]+", " ", text)
  return(text)
}
