#' @export readMortTable
readMortTable <- function(file) {
	testcol <- readr::fwf_empty(file, skip = 3)
	
  mortality.table <- readr::read_fwf(file = file, skip = 3, 
  																	 testcol)
  
  mortality.table <- mortality.table[, colSums(is.na(mortality.table))!=nrow(mortality.table)]
  
  colnames(mortality.table) <- c("Year", "Age", "Female","Male", "Total")
  mortality.table$Total <- gsub("^\\.", "0.", mortality.table$Total)
  mortality.table$Male <- gsub("^\\.", "0.", mortality.table$Male)
  mortality.table$Female <- gsub("^\\.", "0.", mortality.table$Female)
  file.parts <- strsplit(file, '/')[[1]]
  mortality.table$country <- strsplit(file.parts[[length(file.parts)]], "_")[[1]][1]
  mortality.table$Year <- as.numeric(mortality.table$Year)
  mortality.table$Age <- gsub("+", "", mortality.table$Age, fixed = T)
  mortality.table$Age <- as.numeric(mortality.table$Age)
  mortality.table$Female <- as.numeric(mortality.table$Female)
  mortality.table$Male <- as.numeric(mortality.table$Male)
  mortality.table$Total <- as.numeric(mortality.table$Total)
  mortality.table <- dtf(mortality.table)
  
  mortality.table$yob <- mortality.table$Year - mortality.table$Age
  mortality.table <- mortality.table %>% arrange(yob, Year)
  
  mortality.table$Female[which(mortality.table$Female>0.99)] <- 0.98
  mortality.table$Male[which(mortality.table$Male>0.99)] <- 0.98
  return(mortality.table)
}


