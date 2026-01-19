#' Fetch Change.org petition signatures
#'
#' Queries the Change.org API for petition signatures.
#' @param petition.id Petition identifier.
#' @param page Page number to retrieve.
#' @param api_key Change.org API key (defaults to `CHANGE_ORG_API_KEY`).
#' @return A list with signature data.
#' @export
getSignatures <- function(petition.id, page, api_key = Sys.getenv("CHANGE_ORG_API_KEY")) {
	if (api_key == "") {
		stop("Missing Change.org API key. Set CHANGE_ORG_API_KEY or pass api_key.")
	}
	output <- jsonlite::fromJSON(base::readLines(paste0("https://api.change.org/v1/petitions/",
																			petition.id, "/signatures/?page=", page,
																			"page_size=500&api_key=", api_key)))
	output$signatures <- t(simplify2array(output$signatures))
	return(output)
}
