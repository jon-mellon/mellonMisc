test_that("htmlToText extracts text", {
  html <- "<html><body><p>Hello world</p></body></html>"
  out <- htmlToText(html)
  expect_true(grepl("Hello world", out))
})

test_that("getText extracts long sentences", {
  long_text <- paste(rep("word", 60), collapse = " ")
  html <- paste0("<html><body>", long_text, ". Another sentence.</body></html>")
  out <- getText(html)
  expect_true(length(out) >= 1)
})

test_that("listSheets reads example xlsx", {
  file <- readxl::readxl_example("datasets.xlsx")
  sheets <- readxl::excel_sheets(file)
  out <- listSheets(file)
  expect_equal(length(out), length(sheets))
})

test_that("readMortTable parses fixed-width file", {
  file <- file.path(tempdir(), "test_country.txt")
  lines <- c(
    "Header 1",
    "Header 2",
    "Header 3",
    sprintf("%4d %3s %6.2f %6.2f %6.2f", 2010, "0", 0.10, 0.20, 0.30),
    sprintf("%4d %3s %6.2f %6.2f %6.2f", 2011, "1+", 0.11, 0.21, 0.31)
  )
  writeLines(lines, file)
  out <- readMortTable(file)
  expect_equal(nrow(out), 2)
  expect_true(all(c("Year", "Age", "Female", "Male", "Total", "country") %in% names(out)))
  expect_equal(unique(out$country), "test")
})

test_that("read_haven reads extdata .dta", {
  file <- system.file("extdata", "panel_leader.dta", package = "mellonMisc")
  if (file == "") {
    skip("panel_leader.dta not available")
  }
  out <- read_haven(file)
  expect_true(is.data.frame(out))
  expect_true(nrow(out) > 0)
})

test_that("makeSankey writes an html file", {
  switch <- data.frame(
    Origin = c("A", "B"),
    Destination = c("C", "D"),
    Percent = c(10, 20)
  )
  file <- file.path(tempdir(), "sankey_test.html")
  if (file.exists(file)) {
    unlink(file)
  }
  makeSankey(switch, origin.date = "2010", dest.date = "2015", file = file)
  expect_true(file.exists(file))
  expect_true(grepl("<html>", paste(readLines(file), collapse = "\n")))
})

test_that("saveForPub writes outputs and can skip pdf", {
  prefix <- file.path(tempdir(), "save_for_pub_test")
  on.exit(unlink(paste0(prefix, c(".png", ".tiff", ".rda", ".eps", ".svg", ".pdf"))),
          add = TRUE)
  plot <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  saveForPub(plot, prefix, save_pdf = FALSE, silent = TRUE)
  expect_true(file.exists(paste0(prefix, ".rda")))
  expect_true(file.exists(paste0(prefix, ".png")))
  expect_false(file.exists(paste0(prefix, ".pdf")))
})
