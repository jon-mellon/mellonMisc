#' Arrange ggplots with a shared legend
#'
#' Combines multiple ggplot objects into a grid with a single legend.
#' @param ... ggplot objects.
#' @param nrow,ncol Grid layout dimensions.
#' @param position Legend position ("bottom" or "right").
#' @param legend.index Index of plot to draw the legend from.
#' @param left,bottom,right,top Optional annotation grobs.
#' @return A gtable object combining plots and legend.
#' @export
grid_arrange_shared_legend <- function (..., nrow = 1, ncol = NULL,
																				position = c("bottom", "right"), legend.index = 1,
																				left = NULL, bottom = NULL, right = NULL, top = NULL) {
	# browser()
	plots <- list(...)
	position <- match.arg(position)
	if (is.null(ncol)) {
		ncol <- length(plots)
	}
	g <- ggplot2::ggplotGrob(plots[[legend.index]] +
													 ggplot2::theme(legend.position = position))$grobs
	legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
	legend$grobs[[1]]$grobs[[1]]$width[[1]] <- grid::unit(ncol + 5, "npc")
	lheight <- sum(legend$height)
	lwidth <- sum(legend$width)
	gl <- lapply(plots, function(x) x + ggplot2::theme(legend.position = "none"))
	gl <- c(gl, nrow = nrow, ncol = ncol)
	combined <- switch(position, bottom = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
																											legend, ncol = 1,
																											heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight),
																											top = top, bottom = bottom, left = left, right = right),
										 right = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
										 										legend, ncol = 2,
										 										widths = grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth),
										 										top = top, bottom = bottom, left = left, right = right))
	return(combined)
}
