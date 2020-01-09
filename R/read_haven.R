read_haven <- function(file, encoding= NULL) {
  require(haven)
  filetype <- strsplit(file, "\\.")[[1]]
  filetype <- tolower(filetype[length(filetype)])
  if(filetype=="") {
    stop("No filetype listed")
  }
  if(filetype=="sav") {
    data <- read_sav(file)
  }
  if(filetype=="dta") {
  	if(!is.null(encoding)) {
  		data <- read_stata(file, encoding = encoding)	
  	} else {
  		data <- read_stata(file)
  	}
    
  }
  if(filetype=="por") {
    data <- read_por(file)
  }
  if(filetype=="sas") {
    data <- read_sas(file)
  }
  return(data)
}
