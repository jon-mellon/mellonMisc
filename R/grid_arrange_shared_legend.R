#' @export grid_arrange_shared_legend
grid_arrange_shared_legend <- function (..., nrow = 1, ncol = length(plots), 
																				position = c("bottom", "right"), legend.index = 1, 
																				left = NULL, bottom = NULL, right = NULL, top = NULL) {
	# browser()
	plots <- list(...)
	position <- match.arg(position)
	g <- ggplotGrob(plots[[legend.index]] + theme(legend.position = position))$grobs
	legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
	legend$grobs[[1]]$grobs[[1]]$width[[1]] <- grid::unit(ncol + 5, "npc")
	lheight <- sum(legend$height)
	lwidth <- sum(legend$width)
	gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
	gl <- c(gl, nrow = nrow, ncol = ncol)
	combined <- switch(position, bottom = arrangeGrob(do.call(arrangeGrob, gl), 
																										legend, ncol = 1,
																										heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight),
																										top = top, bottom = bottom, left = left, right = right), 
										 right = arrangeGrob(do.call(arrangeGrob, gl), 
										 										legend, ncol = 2, 
										 										widths = grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth),
										 										top = top, bottom = bottom, left = left, right = right))
	return(combined)
}
