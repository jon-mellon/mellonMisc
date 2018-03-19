topBottom <- function(df, row) {
  df2 <- rbind(head(df, row), tail(df, row))
  df2 <- df2[!duplicated(df2), ]
  return(df2)
}