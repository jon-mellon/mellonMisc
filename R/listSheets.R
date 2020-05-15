#' @export listSheets
listSheets <- function(file) {
  sheets<- readxl::excel_sheets(file)  
  sheet.list <- as.list(rep(NA, length(sheets)))
  names(sheet.list) <- sheets
  for(ii in sheets) {
    sheet.list[[ii]] <- readxl::read_excel(file, ii)
  }
  return(sheet.list)
}
