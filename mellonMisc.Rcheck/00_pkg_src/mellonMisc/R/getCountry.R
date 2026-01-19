#' Infer country from location strings
#'
#' Geocodes locations and returns country names.
#' @param locations A character vector of location strings.
#' @return A character vector of country names.
#' @export
getCountry <- function(locations) {
	urls <- paste0("http://www.datasciencetoolkit.org/maps/api/geocode/json?sensor=false 
&address=", sapply(locations, gsub, 
									 pattern = "[[:space:]]+", 
									 replacement = "+"))
	coords <- as.list(rep(NA, length(urls)))
	for(i in 1:length(urls)) {
		coords[[i]] <- base::readLines(urls[i])
	}
	parseCoord <- function(test){
		pos <- which(grepl("location", test))[1]
		if(!is.na(pos)) {
			loc <- test[(pos+1):(pos+2)]
			loc <- gsub("\"|[[:alpha:]]|[, :]", "", loc)
			loc <- as.numeric(loc)
			return(loc)
		} else {
			return(c(NA, NA))
		}
	}
	coords <- t(sapply(coords, parseCoord))
	
	locations <- getLocationFromCoord(latitudes = coords[, 2],
																		longitudes = coords[, 1], 
																		type = "country")
	return(locations)
}
