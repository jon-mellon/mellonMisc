informationToReliability <- function(I) {
  se = 1/sqrt(I)
  1 - (se^2 / (1 + se^2))
}
