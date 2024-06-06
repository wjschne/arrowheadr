library(testthat)
test_that("unitizer", {
  A <- matrix(c(2, 0, 0, 2), ncol = 2, byrow = TRUE)

  expect_identical(unitizer(A), diag(2))
})

test_that("nudger", {
  A <- matrix(0, nrow = 2, ncol = 2)
  E <- matrix(c(0, 0, 1, 1), nrow = 2, ncol = 2)

  expect_identical(nudger(A, c(0, 1)), E)
  expect_identical(nudger(A, 1), matrix(1, nrow = 2, ncol = 2))
  expect_error(nudger(A, c(1, 2, 3)),
               regexp = "The nudge parameter must be a single value or a vector the same length as the number of columns in x.")

})

test_that("rescaler", {
  A <- matrix(1, nrow = 2, ncol = 2)
  E <- matrix(c(2, 2, 2, 2), nrow = 2, ncol = 2)

  expect_identical(rescaler(A, c(2, 2)), E)
  expect_identical(rescaler(A, 2), matrix(2, nrow = 2, ncol = 2))
  expect_error(rescaler(A, c(1, 2, 3)),
               regexp = "The magnitude parameter must be a single value or a vector the same length as the number of columns in x.")

})

test_that("reflecter", {
  A <- diag(c(1, 2))
  E <- matrix(c(1, 0, 0, 1, 0, 2, -2, 0), ncol = 2)
  expect_identical(reflecter(A), E)
  expect_identical(reflecter(A, add_reflection = FALSE),
                   matrix(c(0, 1, -2, 0), ncol = 2))
})


test_that("rotater", {
  expect_identical(round(rotater(matrix(c(0, 1), ncol = 2),
                                 theta = pi), 10),
                   matrix(c(0, -1), ncol = 2))
  expect_error(rotater(matrix(c(0, 1, 1), ncol = 3)),
               "x must be a 2-column matrix or a length-2 vector")

  expect_identical(round(rotater(c(0, 1), theta = pi), 10),
                   matrix(c(0, -1), ncol = 2))
})
