#' Fetch population densities for coordinates
#'
#' Queries the Data Science Toolkit API for population density by coordinate.
#' @param latitudes Numeric vector of latitudes.
#' @param longitudes Numeric vector of longitudes.
#' @param n Chunk size for API requests.
#' @return A numeric vector of population densities.
#' @export
getDensities <- function(latitudes, longitudes, n = 200) {
	if(length(latitudes) != length(longitudes)) {
		stop("Different length vectors.")
	}
	
	coord2stat <- function(x) {
		output <- RCurl::getURL(paste0("http://www.datasciencetoolkit.org/coordinates2statistics/", 
														rjson::toJSON(x), "?statistics=population_density"))
		output <- rjson::fromJSON(output)
		return(output)
	}
	
	all.pos  <- mapply(c, round(latitudes, 3), round(longitudes, 3), SIMPLIFY = FALSE)
	
	n <- ceiling(length(latitudes) / n)
	
	chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
	chunks <- chunk(x = 1:length(all.pos), n = n)
	
	all.output <- as.list(rep(NA, length(chunks)))
	
	for(i in 1:length(chunks)) {
		print(i)
		current.pos <- all.pos[chunks[[i]]]
		temp.output <-  coord2stat(current.pos)	
		all.output[[i]] <- sapply(temp.output, function(x) x$statistics$population_density$value)
	}
	all.output <- do.call(c, all.output)
	return(all.output)
}
