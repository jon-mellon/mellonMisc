codeIssueFragments <- function(text, 
                               substitute = all.subs, 
                               model, doc_matrix) {
  require(tm)
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