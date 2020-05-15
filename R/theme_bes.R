#' @export theme_bes
theme_bes <- function (base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(panel.background = element_rect(fill = "white", 
                                          colour = NA), 
          axis.line = element_line(colour = "grey20"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          legend.key = element_rect(fill = "white", 
                                    colour = NA),
          axis.text=element_text(size=11),
          axis.title=element_text(size=11), 
          complete = FALSE)
}