library(testthat)

# Shared helper: verify the structure every arrowhead function must return.
expect_arrowhead <- function(x, min_rows = 1) {
  expect_true(is.matrix(x))
  expect_equal(ncol(x), 2)
  expect_equal(colnames(x), c("x", "y"))
  expect_gte(nrow(x), min_rows)
}

# Helper: open a temporary PDF device and register cleanup.
with_pdf <- function(code) {
  pdf_path <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_path)
  on.exit({
    grDevices::dev.off()
    unlink(pdf_path)
  }, add = TRUE)
  force(code)
}

# ── arrow_head_bezier ─────────────────────────────────────────────────────────

test_that("arrow_head_bezier returns correct structure", {
  result <- arrow_head_bezier(list(
    c(1, 0, 0.5, 0.5, 0, 1),
    c(0, 1, -0.5, 0.5, -1, 0)
  ))
  expect_arrowhead(result)
})

test_that("arrow_head_bezier straight segments (2 pts) pass through unchanged", {
  # 2-point segments have nrow == 2, not > 2, so bezier() is skipped
  controls <- list(c(0, 0, 1, 0), c(1, 0, 0, 0))
  result <- arrow_head_bezier(controls, n = 51)
  expect_equal(nrow(result), 4)   # 2 rows per segment, 2 segments
})

test_that("arrow_head_bezier curve segments use n points each", {
  # 3-point bezier produces n points per segment
  seg <- c(0, 0, 0.5, 1, 1, 0)   # 3 control points → bezier curve
  result <- arrow_head_bezier(list(seg), n = 51)
  expect_equal(nrow(result), 51)
})

test_that("arrow_head_bezier accepts matrix control points", {
  pts <- matrix(c(1, 0, 0.5, 0.5, 0, 0), ncol = 2, byrow = TRUE)
  result <- arrow_head_bezier(list(pts))
  expect_arrowhead(result)
})

test_that("arrow_head_bezier plot branch runs without error (do.call regression)", {
  # regression: do.call(controls, what = rbind) was the old (broken) form
  with_pdf(
    expect_error(
      arrow_head_bezier(
        list(c(1, 0, 0.5, 0.5, 0, 1), c(0, 1, -0.5, 0.5, -1, 0)),
        plot = TRUE
      ),
      regexp = NA
    )
  )
})

# ── arrow_head_ellipse ────────────────────────────────────────────────────────

test_that("arrow_head_ellipse returns correct structure", {
  expect_arrowhead(arrow_head_ellipse(), min_rows = 100)
})

test_that("arrow_head_ellipse n controls row count", {
  expect_equal(nrow(arrow_head_ellipse(n = 100)), 100)
})

test_that("arrow_head_ellipse unit circle has all points at distance 1", {
  result <- arrow_head_ellipse(n = 361)
  dists <- sqrt(result[, "x"] ^ 2 + result[, "y"] ^ 2)
  expect_true(all(abs(dists - 1) < 1e-8))
})

test_that("arrow_head_ellipse length-2 superellipse works", {
  result <- arrow_head_ellipse(superellipse = c(3, 0.5))
  expect_arrowhead(result)
})

test_that("arrow_head_ellipse different superellipse values change shape", {
  r_iso <- arrow_head_ellipse(superellipse = 3)
  r_ani <- arrow_head_ellipse(superellipse = c(3, 0.5))
  expect_false(identical(r_iso, r_ani))
})

test_that("arrow_head_ellipse superellipse with 3 parameters error", {
  expect_error(arrow_head_ellipse(superellipse = 1:3), "superellipse must be of length 1 or 2")
})

# ── arrow_head_harpoon ────────────────────────────────────────────────────────

test_that("arrow_head_harpoon returns 3-row matrix", {
  result <- arrow_head_harpoon()
  expect_arrowhead(result)
  expect_equal(nrow(result), 3)
})

test_that("arrow_head_harpoon tip is on the positive x-axis with y = 0", {
  result <- arrow_head_harpoon()
  # unitizer normalises, so x < 1, but the tip must stay on the positive x-axis
  expect_gt(as.vector(result[1, "x"]), 0)
  expect_equal(as.vector(result[1, "y"]), 0, tolerance = 1e-10)
})

test_that("arrow_head_harpoon degrees and radians give same result", {
  r_deg <- arrow_head_harpoon(point_angle = 30, barb_angle = 20, degrees = TRUE)
  r_rad <- arrow_head_harpoon(
    point_angle = 30 * pi / 180,
    barb_angle  = 20 * pi / 180,
    degrees = FALSE
  )
  expect_equal(round(r_deg, 10), round(r_rad, 10))
})

# ── arrow_head_hypotrochoid / arrow_head_deltoid ──────────────────────────────

test_that("arrow_head_hypotrochoid returns correct structure", {
  expect_arrowhead(arrow_head_hypotrochoid())
})

test_that("arrow_head_hypotrochoid n controls approximate row count", {
  result <- arrow_head_hypotrochoid(n = 100)
  # n * windings rows (default windings = r = 4), so 400 rows
  expect_equal(nrow(result), 400)
})

test_that("arrow_head_deltoid returns correct structure", {
  expect_arrowhead(arrow_head_deltoid())
})

# ── arrow_head_reuleaux ───────────────────────────────────────────────────────

test_that("arrow_head_reuleaux returns correct structure", {
  expect_arrowhead(arrow_head_reuleaux())
})

test_that("arrow_head_reuleaux always has 361 rows regardless of n", {
  # n is vertex count, not point count; the curve always uses 361 points
  expect_equal(nrow(arrow_head_reuleaux(n = 3)), 361)
  expect_equal(nrow(arrow_head_reuleaux(n = 5)), 361)
})

test_that("arrow_head_reuleaux n = 4 produces different shape from n = 3", {
  r3 <- arrow_head_reuleaux(n = 3)
  r4 <- arrow_head_reuleaux(n = 4)
  expect_false(identical(r3, r4))
})

# ── arrow_head_wittgenstein_rod ───────────────────────────────────────────────

test_that("arrow_head_wittgenstein_rod returns correct structure", {
  expect_arrowhead(arrow_head_wittgenstein_rod())
})

test_that("arrow_head_wittgenstein_rod n controls row count", {
  expect_equal(nrow(arrow_head_wittgenstein_rod(n = 100)), 100)
})

# ── arrow_head_trefoil ────────────────────────────────────────────────────────

test_that("arrow_head_trefoil returns correct structure", {
  expect_arrowhead(arrow_head_trefoil())
})

test_that("arrow_head_trefoil n controls row count", {
  expect_equal(nrow(arrow_head_trefoil(n = 100)), 100)
})

# ── arrow_head_catenary ───────────────────────────────────────────────────────

test_that("arrow_head_catenary closed = TRUE returns correct structure", {
  expect_arrowhead(arrow_head_catenary(closed = TRUE))
})

test_that("arrow_head_catenary closed = FALSE returns correct structure", {
  expect_arrowhead(arrow_head_catenary(closed = FALSE))
})

test_that("arrow_head_catenary closed = FALSE doubles the row count", {
  r_closed <- arrow_head_catenary(n = 50, closed = TRUE)
  r_open   <- arrow_head_catenary(n = 50, closed = FALSE)
  expect_equal(nrow(r_open), 2 * nrow(r_closed))
})

test_that("arrow_head_catenary peakedness parameter a changes shape", {
  r1 <- arrow_head_catenary(a = 1)
  r2 <- arrow_head_catenary(a = 0.4)
  expect_false(identical(r1, r2))
})

# ── arrow_head_latex ──────────────────────────────────────────────────────────

test_that("arrow_head_latex default returns correct structure", {
  expect_arrowhead(arrow_head_latex())
})

test_that("arrow_head_latex with NULL sidecontrols works", {
  expect_arrowhead(arrow_head_latex(sidecontrols = NULL))
})

test_that("arrow_head_latex with NULL undercontrols works", {
  expect_arrowhead(arrow_head_latex(undercontrols = NULL))
})

test_that("arrow_head_latex plot branch runs without error (do.call regression)", {
  # regression: do.call(controls, what = rbind) was the old (broken) form
  with_pdf(
    expect_error(arrow_head_latex(plot = TRUE), regexp = NA)
  )
})

# ── arrow_head_icon ───────────────────────────────────────────────────────────

test_that("arrow_head_icon returns correct structure", {
  expect_arrowhead(arrow_head_icon("eiffel"))
})

test_that("arrow_head_icon works for every preset icon", {
  all_icons <- c(
    "chippenham", "eiffel", "enterprise", "executor", "flame",
    "leaf", "lozenge", "nighthawk", "pantherxf70", "saxon",
    "spade", "stardestoyer", "vikingleaf", "viper", "viper2"
  )
  for (icon in all_icons) {
    result <- arrow_head_icon(icon)
    expect_true(
      is.matrix(result),
      label = paste0("arrow_head_icon('", icon, "') is matrix")
    )
    expect_equal(
      ncol(result), 2,
      label = paste0("arrow_head_icon('", icon, "') has 2 columns")
    )
    expect_equal(
      colnames(result), c("x", "y"),
      label = paste0("arrow_head_icon('", icon, "') colnames are x,y")
    )
  }
})

# ── arrow_head_function ───────────────────────────────────────────────────────

test_that("arrow_head_function default (dnorm) returns correct structure", {
  expect_arrowhead(arrow_head_function())
})

test_that("arrow_head_function closed = FALSE doubles the row count", {
  r_closed <- arrow_head_function(n = 100, closed = TRUE)
  r_open   <- arrow_head_function(n = 100, closed = FALSE)
  expect_equal(nrow(r_open), 2 * nrow(r_closed))
})

test_that("arrow_head_function works with a custom function", {
  result <- arrow_head_function(
    .fun = function(x) x ^ 2,
    lower_bound = -1,
    upper_bound = 1
  )
  expect_arrowhead(result)
})

test_that("arrow_head_function minimum_value adds two extra rows", {
  r_no_min  <- arrow_head_function(n = 100, closed = TRUE)
  r_with_min <- arrow_head_function(n = 100, closed = TRUE, minimum_value = -1)
  # minimum_value prepends and appends one point each → +2 rows
  expect_equal(nrow(r_with_min), nrow(r_no_min) + 2)
})

# ── arrowheads dataset ────────────────────────────────────────────────────────

test_that("arrowheads is a named list", {
  expect_true(is.list(arrowheads))
  expect_false(is.null(names(arrowheads)))
})

test_that("arrowheads contains all expected icon names", {
  expected <- c(
    "chippenham", "eiffel", "enterprise", "executor", "flame",
    "leaf", "lozenge", "nighthawk", "pantherxf70", "saxon",
    "spade", "stardestoyer", "vikingleaf", "viper", "viper2"
  )
  expect_true(all(expected %in% names(arrowheads)))
})

test_that("each arrowheads element is a 2-column matrix", {
  for (nm in names(arrowheads)) {
    m <- arrowheads[[nm]]
    expect_true(
      is.matrix(m),
      label = paste0("arrowheads$", nm, " is matrix")
    )
    expect_equal(
      ncol(m), 2,
      label = paste0("arrowheads$", nm, " has 2 columns")
    )
  }
})
