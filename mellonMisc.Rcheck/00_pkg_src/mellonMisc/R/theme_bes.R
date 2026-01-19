#' BES ggplot theme
#'
#' Provides a simple ggplot theme with a white background.
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @return A ggplot2 theme object.
#' @export
theme_bes <- function (base_size = 11, base_family = "") {
  ggplot2::`%+replace%`(
    ggplot2::theme_grey(base_size = base_size, base_family = base_family),
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = NA),
                   axis.line = ggplot2::element_line(colour = "grey20"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(fill = "white",
                                                      colour = NA),
                   axis.text = ggplot2::element_text(size = 11),
                   axis.title = ggplot2::element_text(size = 11),
                   complete = FALSE)
  )
}
