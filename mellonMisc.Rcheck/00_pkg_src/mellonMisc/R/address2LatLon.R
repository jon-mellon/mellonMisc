#' Geocode addresses via Data Science Toolkit
#'
#' Sends addresses to the Data Science Toolkit API and returns latitude/longitude.
#' @param addresses A character vector of street addresses.
#' @return A data frame with `address`, `lat`, and `lon` columns.
#' @export
address2LatLon <- function(addresses) {
  addresses <- paste(addresses, collapse = "\",\"")
  addresses <- paste0("[\"", addresses, "\"]")
  output <- httr::POST("http://www.datasciencetoolkit.org/street2coordinates",
                       body = addresses, encode = "json")
  output <- httr::content(output, "parsed")
  getLatLon <- function(x) {
    lat <- x["latitude"]
    lon <- x["longitude"]
    lon[is.null(lon)] <- NA
    lat[is.null(lat)] <- NA
    c(lat,lon)
  }
  
  output <- t(sapply(output, getLatLon))
  colnames(output) <- c("lat", "lon")
  output <- dtf(address = rownames(output), output)
  return(output)
}
