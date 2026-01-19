#' BES color palette
#'
#' Returns a vector of British Election Study palette colors.
#' @param n Number of colors to return (max 4).
#' @return A character vector of hex color codes.
#' @export
bes_col <- function(n=4) {
  if(n>4) {
    n <- 4
  }
  return(c("#2b8578", "#971e63",
    "#5d2c7c","#307890")[1:n])
}
