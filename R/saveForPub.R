#' @export saveForPub
saveForPub <- function(object, file.path, width = 6, height = 6, 
											 dpi = 600, text_size  = NULL, silent = TRUE) {
  if(!is.null(text_size)) {
    object <- object + theme(text = element_text(size = text_size), 
                   axis.text = element_text(size = text_size),
                   axis.title = element_text(size = text_size))
  }
  
  try(ggsave(object, file = paste0(file.path, ".png"), 
  					 width = width, height = height, dpi = dpi), silent = silent)
	try(ggsave(object, file = paste0(file.path, ".pdf"), 
						 width = width, height = height, dpi = dpi, 
						 device = cairo_pdf), silent = silent)
  try(ggsave(object, file = paste0(file.path, ".tiff"), 
         width = width, height = height, compression = "lzw", dpi = dpi), 
  		silent = silent)
  try(save(object, file = paste0(file.path, ".rda")), 
  		silent = silent)
  try(ggsave(object, file = paste0(file.path, ".eps"), 
  					 width = width, height = height, dpi = dpi), silent = silent)
  try(ggsave(object, file = paste0(file.path, ".svg"), 
  					 width = width, height = height, dpi = dpi), silent = silent)
  invisible(NULL)
}
