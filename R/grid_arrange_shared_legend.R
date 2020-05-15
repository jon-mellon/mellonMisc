#' @export grid_arrange_shared_legend
grid_arrange_shared_legend <- function (plots, nrow = 1, ncol = length(plots), 
																						 position = c("bottom", "right")) 
{
	browser()
	# plots <- list(...)
	position <- match.arg(position)
	g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
	legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
	legend$grobs[[1]]$grobs[[1]]$width[[1]] <- ncol + 1
	lheight <- sum(legend$height)
	lwidth <- sum(legend$width)
	gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
	gl <- c(gl, nrow = nrow, ncol = ncol)
	combined <- switch(position, bottom = arrangeGrob(do.call(arrangeGrob, gl), 
																										legend, ncol = 1,
																										heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight)), 
										 right = arrangeGrob(do.call(arrangeGrob, gl), 
										 										legend, ncol = 2, 
										 										widths = grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth)))
	return(combined)
}
