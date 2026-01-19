#' Extract text from HTML content
#'
#' Converts HTML to text and returns long sentence fragments.
#' @param file Path or URL passed to `htmlToText()`.
#' @return A character vector of extracted text fragments.
#' @export
getText <- function(file) {
  text <- try(htmlToText(file))
  if(class(text)!="try-error") {
    text <- strsplit(text, "\t")[[1]]
    text <- text[!text %in% c("", " ")]
    text <- text[nchar(text)>200]
    text <- paste(text, collapse = " ")
    
    text <- strsplit(text, "\\.[^[:digit:]]")[[1]]
    
    text <- text[!text %in% c("", " ")]
    text <- text[!grepl("^ +$", text)]
    return(text)
  } else {
    return("")
  }
}
