makeChunkIndex <- function(x, group.size) {
  chunks <- split(1:length(x), ceiling(seq_along(1:length(x)) / group.size) )
}