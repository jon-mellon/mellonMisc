#' Classify issue fragments with a trained model
#'
#' Uses a text model and document matrix to classify text fragments.
#' @param text Character vector of text fragments.
#' @param substitute Optional substitution table passed to `replaceValues`.
#' @param model A trained `RTextTools` model.
#' @param doc_matrix A document-term matrix used to align features.
#' @param bes.defaults Logical; use bundled BES defaults if `TRUE`.
#' @return Classification results from `RTextTools::classify_model`.
#' @export
codeIssueFragments <- function(text, 
                               substitute = NULL, 
                               model = NULL, 
                               doc_matrix = NULL, 
                               bes.defaults = TRUE) {
  if(bes.defaults) {
    data(all.subs, mii.w6.model, mii.w6.mat)
    substitute <- all.subs
    model <- mii.w6.model
    doc_matrix <- mii.w6.mat 
  } else {
    if(is.null(substitute)|is.null(model)|is.null(doc_matrix)) {
      stop("Substitute, model and doc_matrix must be set unless bes.defaults is specified")
    }
  }
  text <- cleanText(text)
  text <- replaceValues(text, subs = substitute)
  new_mat <- create_matrix(text, 
                           language="english", 
                           removeNumbers = TRUE,
                           stemWords = TRUE, 
                           removeSparseTerms = 0.998,
                           ngramLength = 1, 
                           originalMatrix = doc_matrix)
  
  container <- RTextTools::create_container(new_mat, labels = NA, 
                                trainSize = 1:length(text),
                                testSize = 1:length(text),
                                virgin = TRUE)
  predictions <- RTextTools::classify_model(container, model)
  return(predictions)
}
