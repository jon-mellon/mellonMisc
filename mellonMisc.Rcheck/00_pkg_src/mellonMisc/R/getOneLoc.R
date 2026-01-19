#' Geocode a single location string
#'
#' Queries the Data Science Toolkit API or Google Geocoding API for coordinates.
#' @param name A location string.
#' @param google Logical; use Google Geocoding API if `TRUE`.
#' @param api_key Optional Google API key (defaults to `GOOGLE_GEOCODE_API_KEY`).
#' @return A character vector of response lines containing coordinates.
#' @export
getOneLoc <- function(name, google = FALSE, api_key = Sys.getenv("GOOGLE_GEOCODE_API_KEY")) {
	name <- toupper(name)
	if(!google) {
		url <- paste0("http://www.datasciencetoolkit.org/text2places/",
									paste0("[\"", name, "\"]"))
		text <- base::readLines(url)
		text <- text[grepl("latitude|longitude", text)]
		if(length(text)==0) {
			# 		print("failure")
		} else {
		}
		return(text)
	}
	if(google) {
		if (api_key == "") {
			stop("Missing Google API key. Set GOOGLE_GEOCODE_API_KEY or pass api_key.")
		}
		url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?sensor=false&address=", name,"&key=",
									api_key)
		text <- base::readLines(url)
		text <- text[grepl("\"lat\"|\"lng\"", text)][1:2]
		return(text)
	}
}
