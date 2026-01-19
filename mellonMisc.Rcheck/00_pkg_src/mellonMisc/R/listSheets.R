#' Read all sheets from an Excel file
#'
#' Returns a named list of data frames, one per sheet.
#' @param file Path to an Excel file.
#' @return A named list of data frames.
#' @export
listSheets <- function(file) {
  sheets<- readxl::excel_sheets(file)  
  sheet.list <- as.list(rep(NA, length(sheets)))
  names(sheet.list) <- sheets
  for(ii in sheets) {
    sheet.list[[ii]] <- readxl::read_excel(file, ii)
  }
  return(sheet.list)
}
