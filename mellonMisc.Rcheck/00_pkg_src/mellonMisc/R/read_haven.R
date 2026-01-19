#' Read a Haven-supported data file
#'
#' Reads SPSS, Stata, SAS, or POR files using `haven`.
#' @param file Path to a data file.
#' @param encoding Optional encoding for Stata files.
#' @return A data frame.
#' @export
read_haven <- function(file, encoding = NULL) {
  filetype <- strsplit(file, "\\.")[[1]]
  filetype <- tolower(filetype[length(filetype)])
  if(filetype=="") {
    stop("No filetype listed")
  }
  if(filetype=="sav") {
    data <- haven::read_sav(file)
  }
  if(filetype=="dta") {
  	if(!is.null(encoding)) {
  		data <- haven::read_stata(file, encoding = encoding)	
  	} else {
  		data <- haven::read_stata(file)
  	}
    
  }
  if(filetype=="por") {
    data <- haven::read_por(file)
  }
  if(filetype=="sas") {
    data <- haven::read_sas(file)
  }
  return(data)
}
