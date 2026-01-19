#' Format regression output
#'
#' Wrapper around `texreg` to produce HTML or LaTeX tables.
#' @param models A list of model objects.
#' @param output.format Output format: "latex" or "html".
#' @param label Table label for LaTeX output.
#' @param caption Table caption.
#' @return A `texreg` output object.
#' @export
outreg <- function(models, output.format, label, caption) {
  if(output.format=="latex") {
    out <- texreg::texreg(l = models, caption = caption,
                  doctype = FALSE, label = label)
  }
  if(output.format=="html") {
    out <- texreg::htmlreg(l = models, caption = caption, doctype = FALSE)
  }
  return(out)
}
