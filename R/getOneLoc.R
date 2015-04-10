getOneLoc <- function(name, google = FALSE) {
	name <- toupper(name)
	print(name)
	if(!google) {
		url <- paste0("http://www.datasciencetoolkit.org/text2places/",
									paste0("[\"", name, "\"]"))
		text <- readLines(url)
		text <- text[grepl("latitude|longitude", text)]
		if(length(text)==0) {
			# 		print("failure")
		} else {
			print("success")
		}
		return(text)
	}
	if(google) {
		url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?sensor=false&address=", name,"&key=",
									"AIzaSyD7HHcKkrP0TsLzaa_ltmtH1oXQKgHHMKI")
		text <- readLines(url)
		text <- text[grepl("\"lat\"|\"lng\"", text)][1:2]
		return(text)
	}
}
