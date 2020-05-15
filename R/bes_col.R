#' @export bes_col
bes_col <- function(n=4) {
  if(n>4) {
    n <- 4
  }
  return(c("#2b8578", "#971e63",
    "#5d2c7c","#307890")[1:n])
}
