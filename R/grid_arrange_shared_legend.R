grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right"), plots = NULL) {
  
  
  # This one was definitely taken from stack exchange
	#https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
  if(is.null(plots)) {
    plots <- list(...)  
  }
  
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]]+ theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  
  legend$grobs[[1]]$grobs[[1]]$width[[1]] <- ncol + 1
  
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  return(combined)
}