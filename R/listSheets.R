listSheets <- function(file) {
  require(readxl)
  sheets<- excel_sheets(file)  
  sheet.list <- as.list(rep(NA, length(sheets)))
  names(sheet.list) <- sheets
  for(ii in sheets) {
    sheet.list[[ii]] <- read_excel(file, ii)
  }
  return(sheet.list)
}
