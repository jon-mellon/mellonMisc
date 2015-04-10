getLocationFromCoord <- function(latitudes, longitudes,
																 n = 200, type = "county") {
	require(RCurl)
	require(rjson)
	
	if(length(latitudes) != length(longitudes)) {
		stop("Different length vectors.")
	}
	
	coord2pol <- function(x) {
		output <- getURL(paste0("http://www.datasciencetoolkit.org/coordinates2politics/", 
														toJSON(x), ""))
		output <- fromJSON(output)
		return(output)
	}
	
	all.pos  <- mapply(c, round(latitudes, 3), round(longitudes, 3), SIMPLIFY = FALSE)
	# 	n <- 200
	n <- ceiling(length(latitudes) / n)
	
	chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
	chunks <- chunk(x = 1:length(all.pos), n = n)
	
	all.output <- as.list(rep(NA, length(chunks)))
	
	# 	sapply(temp.output, function(x) x$politics[[1]]$name)
	# 	i <- 1
	
	for(i in 1:length(chunks)) {
		print(i)
		current.pos <- all.pos[chunks[[i]]]
		temp.output <-  coord2pol(current.pos)
		
		getType <- function(part) {
			type <- part$politics[sapply(part$politics, function(x) x$friendly_type) == type]
			type <- unlist(type)[3:4]
			return(type)
		}
		temp.output <- lapply(temp.output, getType)
		temp.output[sapply(temp.output, length) == 0 ] <- NA		
		temp.output <- do.call(rbind, temp.output)
		all.output[[i]] <- temp.output
	}
	
	all.output <- do.call(rbind, all.output)
	return(all.output)
}