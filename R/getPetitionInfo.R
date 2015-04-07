getPetitionInfo <- function(petition.id) {
	output <- fromJSON(readLines(paste0("https://api.change.org/v1/petitions/",
																			petition.id, "?",
																			"api_key=",api)))
	return(output)
}