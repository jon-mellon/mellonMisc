getPetitionCreator <- function(petition.id) {
	output <- fromJSON(readLines(paste0("https://api.change.org/v1/petitions/",
																			petition.id, "?",
																			"api_key=",api, 
																			"&fields=creator_url")))
	output <- gsub("[^[:digit:]]", "", output)
	names(output) <- NULL
	return(output)
}
