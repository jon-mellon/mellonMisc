#' @export codeIssueFragments
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
  require(tm)
  require(RTextTools)
  text <- cleanText(text)
  text <- replaceValues(text, subs = substitute)
  new_mat <- create_matrix(text, 
                           language="english", 
                           removeNumbers = TRUE,
                           stemWords = TRUE, 
                           removeSparseTerms = 0.998,
                           ngramLength = 1, 
                           originalMatrix = doc_matrix)
  
  container <- create_container(new_mat, labels = NA, 
                                trainSize = 1:length(text),
                                testSize = 1:length(text),
                                virgin = TRUE)
  predictions <- classify_model(container, model)
  return(predictions)
}
