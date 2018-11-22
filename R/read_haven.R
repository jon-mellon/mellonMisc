read_haven <- function(file) {
  require(haven)
  filetype <- strsplit(file, "\\.")[[1]]
  filetype <- tolower(filetype[length(filetype)])
  if(filetype=="sav") {
    data <- read_sav(file)
  }
  if(filetype=="dta") {
    data <- read_stata(file, encoding = "utf8")
  }
  if(filetype=="por") {
    data <- read_por(file)
  }
  if(filetype=="sas") {
    data <- read_sas(file)
  }
  return(data)
}
