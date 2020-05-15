getSignatures <- function(petition.id, page) {
	output <- jsonlite::fromJSON(readLines(paste0("https://api.change.org/v1/petitions/",
																			petition.id, "/signatures/?page=", page,
																			"page_size=500&api_key=",api)))
	output$signatures <- t(simplify2array(output$signatures))
	return(output)
}
