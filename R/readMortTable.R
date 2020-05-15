#' @export readMortTable
readMortTable <- function(file) {
  mortality.table <- readr::read_fwf(file = file, skip = 3, 
                              fwf_empty(readLines(file, skip = 3)[-1:-3]))
  # mortality.table[substr(mortality.table$X4, 1,9)!=" ", ]
  # mortality.table[substr(mortality.table$X4, 10,11)=="  ", ]$X4
  # 
  # readLines(file, skip = 3)[nchar(readLines(file, skip = 3))!=71]
  # readLines(file, skip = 3)[grepl("\t", readLines(file, skip = 3))]
  
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


