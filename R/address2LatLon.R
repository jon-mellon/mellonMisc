address2LatLon <- function(addresses) {
  require(httr)
  addresses <- paste(addresses, collapse = "\",\"")
  addresses <- paste0("[\"", addresses, "\"]")
  output <- POST('http://www.datasciencetoolkit.org/street2coordinates',
                 body = addresses, encode = 'json')
  output <- content(output, "parsed")
  getLatLon <- function(x) {
    lat <- x["latitude"]
    lon <- x["longitude"]
    lon[is.null(lon)] <- NA
    lat[is.null(lat)] <- NA
    c(lat,lon)
  }
  
  output <- t(sapply(output, getLatLon))
  colnames(output) <- c("lat", "lon")
  output <- dtf(address = names(output), output)
  return(output)
}