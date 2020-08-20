
tif <- function(mod) {
  library(reshape2)
  test <- try(plot(mod, type = "IIC"), silent = T)
  
  tif.total <- rowSums(test[, -1])
  tif.total <- dtf(z = test[, 1], TIF = tif.total)
  tif.total <- ggplot(tif.total, aes(x = z, y = TIF)) + geom_line() + theme_bes() + 
    ylab("Information") + xlab("Theta")  + 
    geom_hline(yintercept = 2.4, colour = "red", linetype = 3)
  
  tif <- dtf(as.matrix(test))
  
  tif <- melt(tif, id.vars = "z")  
  colnames(tif) <- c("Theta", "Item", "Information")
  iif <- ggplot( tif, aes(x = Theta, y = Information, group = Item, colour = Item, linetype = Item)) + 
    geom_line() + theme_bes()
  return(list(iif=iif, tif.total=tif.total))
}