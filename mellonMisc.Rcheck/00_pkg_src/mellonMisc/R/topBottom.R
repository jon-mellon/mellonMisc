#' Extract top and bottom rows
#'
#' Returns the first and last `row` rows of a data frame.
#' @param df A data frame.
#' @param row Number of rows to take from top and bottom.
#' @return A data frame of selected rows.
#' @export
topBottom <- function(df, row) {
  df2 <- rbind(head(df, row), tail(df, row))
  df2 <- df2[!duplicated(df2), , drop = FALSE]
  return(df2)
}
