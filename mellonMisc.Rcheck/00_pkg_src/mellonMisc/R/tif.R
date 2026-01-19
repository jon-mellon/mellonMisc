#' Plot item and test information functions
#'
#' Creates item information function (IIF) and test information function (TIF)
#' plots from an IRT model.
#' @param mod A fitted IRT model with `plot(..., type = "IIC")` support.
#' @return A list with `iif` and `tif.total` ggplot objects.
#' @export
tif <- function(mod) {
  test <- try(plot(mod, type = "IIC"), silent = TRUE)
  
  tif.total <- rowSums(test[, -1])
  tif.total <- dtf(z = test[, 1], TIF = tif.total)
  tif.total <- ggplot2::ggplot(tif.total, ggplot2::aes(x = z, y = TIF)) +
    ggplot2::geom_line() + theme_bes() +
    ggplot2::ylab("Information") + ggplot2::xlab("Theta") +
    ggplot2::geom_hline(yintercept = 2.4, colour = "red", linetype = 3)
  
  tif <- dtf(as.matrix(test))
  
  tif <- reshape2::melt(tif, id.vars = "z")
  colnames(tif) <- c("Theta", "Item", "Information")
  iif <- ggplot2::ggplot(tif, ggplot2::aes(x = Theta, y = Information, group = Item,
                                          colour = Item, linetype = Item)) +
    ggplot2::geom_line() + theme_bes()
  return(list(iif = iif, tif.total = tif.total))
}
