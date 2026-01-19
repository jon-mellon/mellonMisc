test_that("logitmfx helpers return expected structures", {
  set.seed(1)
  df <- data.frame(
    y = rbinom(20, 1, 0.5),
    x = rnorm(20)
  )
  res <- logitmfxest.w(y ~ x, data = df, weights = rep(1, nrow(df)))
  expect_true(inherits(res$fit, "glm"))
  expect_true(is.data.frame(res$mfx))

  res2 <- logitmfx.w(y ~ x, data = df, weights = rep(1, nrow(df)))
  expect_true(inherits(res2, "logitmfx"))
  expect_true(is.matrix(res2$mfxest))
})

test_that("robustSEs returns a coeftest", {
  df <- data.frame(x = 1:10, y = 1:10 + rnorm(10))
  fit <- lm(y ~ x, data = df)
  out <- robustSEs(fit)
  expect_true(inherits(out, "coeftest"))
})

test_that("predictAt returns probabilities and intervals", {
  set.seed(2)
  df <- data.frame(
    y = rbinom(30, 1, 0.4),
    x = rnorm(30),
    w = runif(30, 1, 2)
  )
  mod <- glm(y ~ x, data = df, weights = w, family = binomial())
  out <- predictAt(mod, at = list(x = c(-1, 0, 1)))
  expect_true(all(c("prob", "se", "lci", "uci") %in% names(out)))
  expect_equal(nrow(out), 3)
})

test_that("predict.clogit computes stratum probabilities", {
  obj <- structure(list(coefficients = c(`(Intercept)` = 0.1, x = 0.5)),
                   class = "clogit")
  newdata <- data.frame(
    y = c(0, 0),
    x = c(0.2, 0.8),
    strata = c(1, 1)
  )
  probs <- predict.clogit(obj, newdata = newdata, formula = y ~ x, strata.var = "strata")
  expect_equal(sum(probs), 1, tolerance = 1e-8)
  expect_error(predict.clogit(obj, newdata = newdata, type = "link",
                              formula = y ~ x, strata.var = "strata"))
})

test_that("calcInfo returns a bounded reliability estimate", {
  testthat::skip_if_not_installed("ltm")
  set.seed(1)
  dat <- data.frame(item1 = sample(1:3, 30, TRUE),
                    item2 = sample(1:3, 30, TRUE))
  mod <- ltm::grm(dat)
  out <- calcInfo(mod, dat)
  expect_true(is.numeric(out))
  expect_true(length(out) == 1)
  expect_true(is.finite(out))
  expect_true(out >= 0 && out <= 1)
})

test_that("predGRMScores returns scores for a fitted GRM", {
  testthat::skip_if_not_installed("ltm")
  set.seed(1)
  dat <- data.frame(item1 = sample(1:3, 30, TRUE),
                    item2 = sample(1:3, 30, TRUE))
  mod <- ltm::grm(dat)
  out <- predGRMScores(dat, mod)
  expect_true(all(c("z", "se") %in% names(out)))
  expect_equal(nrow(out), nrow(dat))
})

test_that("makeDesign creates a survey design", {
  df <- data.frame(id = 1:5, w = 1:5, x = rnorm(5))
  des <- makeDesign(df, weight.var = "w", id.var = "id")
  expect_true(inherits(des, "survey.design"))
})

test_that("outreg returns output", {
  fit <- lm(mpg ~ hp, data = mtcars)
  out_latex <- outreg(list(fit), output.format = "latex", label = "tab1", caption = "Cap")
  out_html <- outreg(list(fit), output.format = "html", label = "tab1", caption = "Cap")
  expect_false(is.null(out_latex))
  expect_false(is.null(out_html))
})
