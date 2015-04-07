getPetitionID <- function(petition.url) {
	output <- fromJSON(readLines(paste0("https://api.change.org/v1/petitions/get_id?api_key=",
																			api,"&petition_url=",
																			petition.url)))
	return(output)
}