library(testthat)

# ── unitizer ──────────────────────────────────────────────────────────────────

test_that("unitizer scales 2x identity to identity", {
  A <- matrix(c(2, 0, 0, 2), ncol = 2, byrow = TRUE)
  expect_identical(unitizer(A), diag(2))
})

test_that("unitizer max distance from origin is 1", {
  A <- matrix(c(1, 2, -8, 6, 9, 5), ncol = 2, byrow = TRUE)
  result <- unitizer(A)
  dists <- apply(result, 1, function(r) sqrt(sum(r ^ 2)))
  expect_equal(max(dists), 1)
})

test_that("unitizer respects custom center", {
  A <- matrix(c(1, 2, -8, 6, 9, 5), ncol = 2, byrow = TRUE)
  result <- unitizer(A, center = colMeans(A))
  dists <- apply(result, 1, function(r) sqrt(sum(r ^ 2)))
  expect_equal(max(dists), 1)
})

test_that("unitizer preserves colnames", {
  A <- matrix(c(2, 0, 0, 2), ncol = 2, byrow = TRUE,
              dimnames = list(NULL, c("x", "y")))
  expect_equal(colnames(unitizer(A)), c("x", "y"))
})

# ── nudger ────────────────────────────────────────────────────────────────────

test_that("nudger shifts columns by vector", {
  A <- matrix(0, nrow = 2, ncol = 2)
  expect_identical(nudger(A, c(0, 1)), matrix(c(0, 0, 1, 1), nrow = 2))
  expect_identical(nudger(A, 1), matrix(1, nrow = 2, ncol = 2))
})

test_that("nudger zero nudge is identity", {
  A <- matrix(c(1, 2, 3, 4), ncol = 2)
  expect_identical(nudger(A, c(0, 0)), A)
})

test_that("nudger errors on wrong-length nudge", {
  A <- matrix(0, nrow = 2, ncol = 2)
  expect_error(
    nudger(A, c(1, 2, 3)),
    "The nudge parameter must be a single value or a vector the same length as the number of columns in x."
  )
})

test_that("nudger preserves colnames", {
  A <- matrix(0, nrow = 2, ncol = 2, dimnames = list(NULL, c("x", "y")))
  expect_equal(colnames(nudger(A, c(1, 2))), c("x", "y"))
})

# ── rescaler ──────────────────────────────────────────────────────────────────

test_that("rescaler scales columns by vector", {
  A <- matrix(1, nrow = 2, ncol = 2)
  expect_identical(rescaler(A, c(2, 2)), matrix(c(2, 2, 2, 2), nrow = 2))
  expect_identical(rescaler(A, 2), matrix(2, nrow = 2, ncol = 2))
})

test_that("rescaler identity magnitude is identity", {
  A <- matrix(c(1, 2, 3, 4), ncol = 2)
  expect_identical(rescaler(A, c(1, 1)), A)
})

test_that("rescaler errors on wrong-length magnitude", {
  A <- matrix(1, nrow = 2, ncol = 2)
  expect_error(
    rescaler(A, c(1, 2, 3)),
    "The magnitude parameter must be a single value or a vector the same length as the number of columns in x."
  )
})

test_that("rescaler preserves colnames", {
  A <- matrix(c(1, 2, 3, 4), ncol = 2, dimnames = list(NULL, c("x", "y")))
  expect_equal(colnames(rescaler(A, c(2, 3))), c("x", "y"))
})

# ── reflecter ─────────────────────────────────────────────────────────────────

test_that("reflecter with add_reflection = TRUE appends negated rows", {
  A <- diag(c(1, 2))
  E <- matrix(c(1, 0, 0, 1, 0, 2, -2, 0), ncol = 2)
  expect_identical(reflecter(A), E)
})

test_that("reflecter with add_reflection = FALSE returns only reflection", {
  A <- diag(c(1, 2))
  expect_identical(
    reflecter(A, add_reflection = FALSE),
    matrix(c(0, 1, -2, 0), ncol = 2)
  )
})

test_that("reflecter doubles row count when add_reflection = TRUE", {
  A <- matrix(c(1, 0.3, 0.5, 0.6, 0, 0), ncol = 2, byrow = TRUE)
  expect_equal(nrow(reflecter(A)), 2 * nrow(A))
})

test_that("reflecter negates y-coordinates in reflection", {
  A <- matrix(c(1, 0.3, 0.5, 0.6), ncol = 2, byrow = TRUE)
  ref <- reflecter(A, add_reflection = FALSE)
  expect_equal(ref[, 2], -rev(A[, 2]))
})

# ── rotater ───────────────────────────────────────────────────────────────────

test_that("rotater 180 degrees negates y (and leaves x near 0)", {
  result <- round(rotater(matrix(c(0, 1), ncol = 2), theta = pi), 10)
  expect_identical(result, matrix(c(0, -1), ncol = 2))
})

test_that("rotater accepts vector input", {
  result <- round(rotater(c(0, 1), theta = pi), 10)
  expect_identical(result, matrix(c(0, -1), ncol = 2))
})

test_that("rotater 90 degrees moves (1,0) to (0,1)", {
  result <- round(rotater(matrix(c(1, 0), ncol = 2), theta = pi / 2), 10)
  expect_equal(result[1, 1], 0)
  expect_equal(result[1, 2], 1)
})

test_that("rotater degrees mode matches radians mode", {
  r_rad <- round(rotater(matrix(c(1, 0), ncol = 2), theta = pi / 2), 10)
  r_deg <- round(rotater(matrix(c(1, 0), ncol = 2), theta = 90, degrees = TRUE), 10)
  expect_equal(r_rad, r_deg)
})

test_that("rotater with 1-row matrix center expands to match x rows", {
  # regression: nrow(center == 1) was wrong; should be nrow(center) == 1
  x <- matrix(c(1, 0, 0, 1, -1, 0, 0, -1), ncol = 2, byrow = TRUE)
  result_mat   <- rotater(x, theta = pi / 2, center = matrix(c(0.5, 0.5), ncol = 2))
  result_vec   <- rotater(x, theta = pi / 2, center = c(0.5, 0.5))
  expect_equal(result_mat, result_vec)
})

test_that("rotater errors when matrix center row count mismatches x", {
  x <- matrix(c(1, 0, 0, 1, -1, 0), ncol = 2, byrow = TRUE)   # 3 rows
  bad_center <- matrix(c(0, 0, 1, 1), ncol = 2, nrow = 2)      # 2 rows
  expect_error(
    rotater(x, theta = pi, center = bad_center),
    "center must have 1 row or the same number of rows as x"
  )
})

test_that("rotater errors on wrong ncol for x", {
  expect_error(
    rotater(matrix(c(0, 1, 1), ncol = 3)),
    "x must be a 2-column matrix or a length-2 vector"
  )
})

test_that("rotater errors on wrong-length center vector", {
  expect_error(
    rotater(matrix(c(1, 0), ncol = 2), theta = pi, center = c(1, 2, 3)),
    "center must be a 2-column matrix or a length-2 vector"
  )
})

test_that("rotater errors on wrong-length vector", {
  expect_error(
    rotater(c(1, 0, 2), theta = pi, center = c(1, 2, 3)),
    "x must be a 2-column matrix or a length-2 vector"
  )
})

test_that("rotater preserves colnames", {
  x <- matrix(c(1, 0), ncol = 2, dimnames = list(NULL, c("x", "y")))
  expect_equal(colnames(rotater(x, theta = pi / 4)), c("x", "y"))
})

# ── transformer ───────────────────────────────────────────────────────────────

test_that("transformer applies nudge correctly", {
  xy <- matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE,
               dimnames = list(NULL, c("x", "y")))
  result <- transformer(xy, transformations = "nudger", nudge = c(1, 2))
  expect_equal(result, nudger(xy, c(1, 2)))
})

test_that("transformer applies rotate correctly", {
  xy <- matrix(c(1, 0, 0, 1), ncol = 2, byrow = TRUE)
  result   <- round(transformer(xy, transformations = "rotater", rotate = pi), 10)
  expected <- round(rotater(xy, theta = pi), 10)
  expect_equal(result, expected)
})

test_that("transformer order matters", {
  xy <- matrix(c(1, 0, 0, 1), ncol = 2, byrow = TRUE,
               dimnames = list(NULL, c("x", "y")))
  # nudge-then-rescale != rescale-then-nudge
  r1 <- transformer(xy, nudge = c(1, 0), rescale = c(2, 1),
                    transformations = c("nudger", "rescaler"))
  r2 <- transformer(xy, nudge = c(1, 0), rescale = c(2, 1),
                    transformations = c("rescaler", "nudger"))
  expect_false(identical(r1, r2))
})

test_that("transformer preserves colnames", {
  xy <- matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE,
               dimnames = list(NULL, c("x", "y")))
  result <- transformer(xy, transformations = "nudger", nudge = c(1, 2))
  expect_equal(colnames(result), c("x", "y"))
})

# ── v2matrix ──────────────────────────────────────────────────────────────────

test_that("v2matrix returns 2-column matrix with x,y names", {
  result <- v2matrix(c(1, 2, 3, 4))
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("x", "y"))
})

test_that("v2matrix byrow = TRUE places pairs as rows", {
  result <- v2matrix(c(1, 2, 3, 4))
  expect_equal(result[1, ], c(x = 1, y = 2))
  expect_equal(result[2, ], c(x = 3, y = 4))
})

test_that("v2matrix ncol != 2 uses x1,x2,... names", {
  result <- v2matrix(c(1, 2, 3, 4, 5, 6), ncol = 3)
  expect_equal(colnames(result), c("x1", "x2", "x3"))
})

# ── rev_matrix_rows / rev_matrix_cols ─────────────────────────────────────────

test_that("rev_matrix_rows reverses row order", {
  A <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2, byrow = TRUE)
  result <- rev_matrix_rows(A)
  expect_equal(result[1, ], A[3, ])
  expect_equal(result[3, ], A[1, ])
})

test_that("rev_matrix_rows of single row returns the same values", {
  # R drops dimensions for single-row subscripting, so result is a vector
  A <- matrix(c(3, 7), ncol = 2)
  expect_equal(as.vector(rev_matrix_rows(A)), c(3, 7))
})

test_that("rev_matrix_cols reverses column order", {
  A <- matrix(c(1, 2, 3, 4), ncol = 2, byrow = TRUE)
  result <- rev_matrix_cols(A)
  expect_equal(result[, 1], A[, 2])
  expect_equal(result[, 2], A[, 1])
})
