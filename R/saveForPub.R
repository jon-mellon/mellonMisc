#' Save a ggplot in multiple publication formats
#'
#' Writes a plot to PNG, TIFF, RDA, EPS, and SVG, with optional PDF output.
#' @param object A ggplot object to save.
#' @param file.path Output path without an extension.
#' @param width,height Plot size in inches.
#' @param dpi Resolution for raster outputs.
#' @param text_size Optional text size override for plot theme.
#' @param silent Logical; suppress errors from graphics device calls.
#' @param save_pdf Logical; whether to save a PDF file.
#' @param include_pdf Deprecated alias for `save_pdf`.
#' @return `NULL`, invisibly.
#' @export
saveForPub <- function(object, file.path, width = 6, height = 6,
                       dpi = 600, text_size = NULL, silent = TRUE,
                       save_pdf = FALSE, include_pdf = NULL, include_eps = NULL) {
  if (!is.null(include_pdf)) {
    save_pdf <- isTRUE(include_pdf)
  }
	if (!is.null(include_eps)) {
		save_eps <- isTRUE(include_eps)
	}
  if (!is.null(text_size)) {
    object <- object + ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      axis.text = ggplot2::element_text(size = text_size),
      axis.title = ggplot2::element_text(size = text_size)
    )
  }
	try(ggplot2::ggsave(object, file = paste0(file.path, ".svg"),
											width = width, height = height, dpi = dpi,  device = svglite::svglite),
			silent = silent)
  try(ggplot2::ggsave(object, file = paste0(file.path, ".png"),
                      width = width, height = height, dpi = dpi),
      silent = silent)
  if (save_pdf) {
    try(ggplot2::ggsave(object, file = paste0(file.path, ".pdf"),
                        width = width, height = height, dpi = dpi,
                        device = grDevices::cairo_pdf),
        silent = silent)
  }
  try(ggplot2::ggsave(object, file = paste0(file.path, ".tiff"),
                      width = width, height = height, compression = "lzw",
                      dpi = dpi),
      silent = silent)
  try(save(object, file = paste0(file.path, ".rda")),
      silent = silent)
	if(save_eps) {
		try(ggplot2::ggsave(object, file = paste0(file.path, ".eps"),
												width = width, height = height, dpi = dpi),
				silent = silent)	
	}

  invisible(NULL)
}
