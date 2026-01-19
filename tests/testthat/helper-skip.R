skip_if_no_network <- function() {
  if (!nzchar(Sys.getenv("MELLONMISC_RUN_NETWORK_TESTS"))) {
    testthat::skip("Network tests disabled")
  }
}
