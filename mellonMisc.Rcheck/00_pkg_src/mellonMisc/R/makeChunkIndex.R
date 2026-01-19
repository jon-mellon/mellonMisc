#' Build chunk indices
#'
#' Splits indices of a vector into groups of a fixed size.
#' @param x A vector to index.
#' @param group.size Size of each chunk.
#' @return A list of integer index vectors.
#' @export
makeChunkIndex <- function(x, group.size) {
  chunks <- split(1:length(x), ceiling(seq_along(1:length(x)) / group.size) )
}
