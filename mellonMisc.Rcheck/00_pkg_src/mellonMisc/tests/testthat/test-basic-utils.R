test_that("duplicate helpers and zeroOrNA work", {
  x <- c(1, 2, 2, 3, 1)
  expect_equal(allDup(x), c(TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(dupEither(x), c(TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(zeroOrNA(c(0, 1, NA)), c(TRUE, FALSE, TRUE))
})

test_that("row-wise helpers work", {
  a <- c(1, NA, 3)
  b <- c(4, 5, 6)
  expect_equal(sumByRow(a, b, na.rm = TRUE), c(5, 5, 9))
  expect_equal(meanByRow(a, b, na.rm = TRUE), c(2.5, 5, 4.5))
  expect_equal(minByRow(a, b, na.rm = TRUE), c(1, 5, 3))
  expect_equal(maxByRow(a, b, na.rm = TRUE), c(4, 5, 6))
})

test_that("vector helpers work", {
  tab <- table(c("a", "b", "a"))
  expect_equal(unname(tToV(tab)), c(2, 1))
  expect_error(tToV(matrix(1:4, nrow = 2)))
  expect_equal(p2p(0.123, round = 1), 12.3)
  expect_equal(as.character(excelWinDate(1)), "1899-12-31")
})

test_that("string helpers work", {
  expect_equal(writeSentenceList(c("a", "b", "c")), "a, b, and c")
  expect_equal(writeSentenceList(c("a", "b")), "a and b")
  expect_equal(orderWords("b a"), "a b")
  expect_equal(unname(formatStandard("City of York Council")), "York")
  expect_equal(getMiddleInitial(c("John Q Public", "Jane Public")), c("q", NA))
  expect_equal(getPostcode("AB1 2CD"), c(area = "AB1", postcode = "AB1 2CD"))
  expect_equal(gbPtyCol(c("Labour", "Conservative")),
               c(Labour = "#d50000", Conservative = "#0087dc"))
  expect_equal(bes_col(2), c("#2b8578", "#971e63"))
  expect_equal(trimws(cleanText("Hello, World! 123")), "hello world")
  subs <- matrix(c("a", "b", "x", "y"), ncol = 2, byrow = TRUE)
  expect_equal(replaceValues("a x c", subs), "b y c")
})

test_that("dtf and list helpers work", {
  df <- dtf(a = 1, b = "x", StAsFa = TRUE)
  expect_true(is.factor(df$b))
  df2 <- data.frame(a = 1:5)
  expect_equal(nrow(topBottom(df2, 2)), 4)
  expect_equal(unname(makeChunkIndex(1:5, 2)), list(1:2, 3:4, 5))
})

test_that("factor and adjacency helpers work", {
  x <- factor(c("a", "a", "b", "b"))
  y <- factor(c("x", "y", "x", "y"))
  expect_equal(as.character(factorInteractions(x, y)),
               c("a x", "a y", "b x", "b y"))

  df <- data.frame(country = c("A", "B"), val = c(1, 2))
  adj <- varToAdjacency(df, "val", "country", c("A", "B"), col = FALSE)
  expect_equal(unname(diag(adj)), c(1, 2))
  adj2 <- varToAdj(df, "val", "country", c("A", "B"), col = FALSE)
  expect_equal(unname(diag(adj2)), c(1, 2))

  adjacency <- matrix(c(0, 2, 2, 0), nrow = 2, byrow = TRUE)
  rownames(adjacency) <- colnames(adjacency) <- c("A", "B")
  edge <- adjacencyToEdgeList(adjacency)
  expect_true(all(edge$value == 2))

  edges <- data.frame(from = c("A", "A"), to = c("B", "C"), weight = c(1, 2))
  adj_from_edges <- edgeListToAdjacency(edges)
  expect_equal(adj_from_edges["A", "B"], 1)
  adj_weighted <- convertEdgeToAdjacencyWeighted(edges)
  expect_equal(adj_weighted["A", "C"], 2)
})

test_that("co-occurrence and distance helpers work", {
  tdm <- matrix(c(1, 0, 1, 1, 0, 1), nrow = 3, byrow = TRUE)
  colnames(tdm) <- c("a", "b")
  adj <- cooccurenceToAdjacency(tdm)
  expect_equal(dim(adj), c(2, 2))
  expect_equal(adj["a", "b"], 1)

  points <- matrix(c(0, 0, 0, 0), ncol = 2, byrow = TRUE)
  expect_equal(manyDistToLoc(points, c(0, 0)), c(0, 0))
})

test_that("aggregation and merge helpers work", {
  df <- data.frame(id = c("a", "b", "c"), value = c(1, 2, 3), w = c(1, 2, 1))
  lookup <- data.frame(old = c("a", "b", "c"), new = c("x", "x", "y"))
  agg <- aggregateToBoundaries(df, "id", "w", lookup)
  expect_equal(agg$value[agg$identifier.y == "x"], 5 / 3)

  trans <- matrix(c(0.5, 0.5, 0.2, 0.8), nrow = 2, byrow = TRUE)
  rownames(trans) <- c("A", "B")
  colnames(trans) <- c("X", "Y")
  totals <- assignToBoundaries(trans, c(10, 20), c("A", "B"))
  expect_equal(totals, c(X = 9, Y = 21))

  x <- data.frame(id = c(1, 1), a = c(1, 2))
  y <- data.frame(id = 1, b = 3)
  expect_error(safemerge(x, y, by = "id", type = "1:1"))
})

test_that("miscellaneous helpers work", {
  expect_equal(informationToReliability(1), 0.5)
  expect_equal(reconcileStrings(c("Alpha", "Beta"), c("Alpha", "Beta")),
               c("Alpha", "Beta"))
  res <- spellCorrect("hello helo hello")
  expect_true(is.list(res))

  df <- data.frame(a = 1, b = 2)
  expect_true("c" %in% names(rnm(df, c = a)))
  expect_equal(names(slct(df, a)), "a")
})

test_that("convertOccs maps crosswalks", {
  data("occ.crosswalks")
  cw_name <- names(occ.crosswalks)[1]
  parts <- strsplit(cw_name, "-")[[1]]
  from <- parts[1]
  to <- parts[2]
  cw <- occ.crosswalks[[cw_name]]
  cw <- cw[!is.na(cw[, 1]) & !is.na(cw[, 2]), , drop = FALSE]
  occs <- cw[1:5, 1]
  expected <- cw[1:5, 2]
  expect_equal(convertOccs(occs, from, to), expected)
})
