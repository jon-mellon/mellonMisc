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