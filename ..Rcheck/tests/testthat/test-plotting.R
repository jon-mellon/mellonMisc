test_that("plotDiscrete returns a ggplot", {
  df <- data.frame(var = factor(c("a", "b", "a")))
  p <- plotDiscrete("var", df)
  expect_true(inherits(p, "ggplot"))
})

test_that("theme_bes returns a theme", {
  th <- theme_bes()
  expect_true(inherits(th, "theme"))
})

test_that("grid_arrange_shared_legend combines plots", {
  p1 <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(data.frame(x = 1, y = 2), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  out <- grid_arrange_shared_legend(p1, p2)
  expect_true(inherits(out, "gtable"))
})

test_that("tif builds information plots with a mocked plot call", {
  fake_plot <- function(mod, type = "IIC", ...) {
    cbind(z = c(-1, 0, 1), item1 = c(1, 2, 3), item2 = c(2, 1, 0))
  }
  out <- testthat::with_mocked_bindings(
    tif(structure(list(), class = "fake_model")),
    plot = fake_plot,
    .env = asNamespace("mellonMisc")
  )
  expect_true(inherits(out$iif, "ggplot"))
  expect_true(inherits(out$tif.total, "ggplot"))
})

test_that("resetPar restores graphics parameters", {
  old_device <- getOption("device")
  options(device = function(...) pdf(file = tempfile(fileext = ".pdf"), ...))
  on.exit(options(device = old_device), add = TRUE)
  out <- resetPar()
  expect_true(is.list(out))
})
