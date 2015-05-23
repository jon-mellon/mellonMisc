cleanText <- function(text) {
  text <- tolower(text)
  #   text[!text %in% c("__na__", "", " na ")] <- NA
  text <- gsub("[[:punct:][:digit:]]", " ", text)
  text <- gsub("\\\r", " ", text)
  text <- gsub("\\\n", " ", text)
  text <- gsub("[[:space:]]+", " ", text)
  return(text)
}