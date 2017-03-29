safemerge <- function(x, y, by = intersect(names(x), names(y)),
                      by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
                      sort = TRUE, suffixes = c(".x",".y"),
                      incomparables = NULL, 
                      type = c("1:1", "m:1", "1:m", "m:m"), 
                      merge.on.na = FALSE, 
                      ...) {
  # this function is safe merge
  type <- type[1]
  if(type %in% c("1:1", "1:m")) {
    if(any(duplicated(x[, by.x]) & complete.cases(x[, by.x]))) {
      error <- paste("Merge type defined as ", type, " but ",
                     by.x, " does not identify observations in x uniquely")
      stop(error)
    }
  }
  if(type %in% c("1:1", "m:1")) {
    if(any(duplicated(y[, by.y]) & complete.cases(y[, by.y])   )) {
      error <- paste("Merge type defined as ", type, " but ",
                     by.y, " does not identify observations in y uniquely")
      stop(error)
    }
  }
  if(!merge.on.na) {
    if(any(is.na(y[, by.y])) & any(is.na(x[, by.x]))) {
      error <- paste("merge.on.na=FALSE but there are NA values in x and y, this will lead to multiplying cases. If you intended to merge on NAs, set merge.on.na to TRUE")
      stop(error)
    }
  }
  
  output <- merge(x, y, by.x = by.x, by.y = by.y, 
                  all.x = all.x, all.y = all.y,
                  sort = sort, suffixes = suffixes,
                  incomparables = incomparables)
  return(output)
}