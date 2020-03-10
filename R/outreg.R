outreg <- function(models, output.format, label, caption) {
  if(output.format=="latex") {
    out <- texreg::texreg(l = models, caption = caption,
                  doctype = F, label = label)
  }
  if(output.format=="html") {
    out <- texreg::htmlreg(l = models, caption = caption, doctype = F)
  }
  return(out)
}