test_that("address2LatLon can be skipped or queried", {
  skip_if_no_network()
  out <- address2LatLon(c("10 Downing St, London"))
  expect_true(is.data.frame(out))
  expect_true(all(c("lat", "lon") %in% names(out)))
})

test_that("getCountry can be skipped or queried", {
  skip_if_no_network()
  out <- getCountry(c("Manchester, UK"))
  expect_equal(length(out), 1)
})

test_that("getDensities validates inputs", {
  expect_error(getDensities(c(1, 2), c(1)))
})

test_that("getLocationFromCoord validates inputs", {
  expect_error(getLocationFromCoord(c(1, 2), c(1)))
})

test_that("getOneLoc errors without a Google API key", {
  expect_error(getOneLoc("London", google = TRUE, api_key = ""))
})

test_that("getOneLoc non-Google lookup can be skipped or queried", {
  skip_if_no_network()
  out <- getOneLoc("London", google = FALSE)
  expect_true(length(out) > 0)
})

test_that("getSignatures errors without an API key", {
  expect_error(getSignatures(123, 1, api_key = ""))
})
