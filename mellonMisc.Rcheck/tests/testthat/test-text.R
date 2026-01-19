test_that("create_matrix builds a document-term matrix", {
  texts <- c("this is a test", "this is another test")
  mat <- create_matrix(texts, removeStopwords = FALSE)
  expect_true(inherits(mat, "DocumentTermMatrix"))
  expect_equal(nrow(mat), length(texts))
})

test_that("codeIssueFragments errors without required inputs", {
  expect_error(codeIssueFragments("test", bes.defaults = FALSE))
})
