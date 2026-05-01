test_that("hex_to_rgba converts known colours", {
  expect_equal(hex_to_rgba("#000000", 1.0),  "rgba(0,0,0,1.00)")
  expect_equal(hex_to_rgba("#FFFFFF", 1.0),  "rgba(255,255,255,1.00)")
  expect_equal(hex_to_rgba("#FF0000", 0.5),  "rgba(255,0,0,0.50)")
  expect_equal(hex_to_rgba("#0080FF", 0.25), "rgba(0,128,255,0.25)")
})

test_that("hex_to_rgba accepts hex without leading #", {
  expect_equal(hex_to_rgba("00FF00", 1.0), "rgba(0,255,0,1.00)")
})

test_that("hex_to_rgba is case-insensitive", {
  expect_equal(hex_to_rgba("#abcdef", 1.0), hex_to_rgba("#ABCDEF", 1.0))
})

test_that("compute_line_diff returns NULL for NULL inputs", {
  expect_null(compute_line_diff(NULL, "x"))
  expect_null(compute_line_diff("x",  NULL))
  expect_null(compute_line_diff(NULL, NULL))
})

test_that("compute_line_diff detects added and removed lines", {
  d <- compute_line_diff("line1\nline2\nline3", "line1\nlineX\nline3")
  expect_equal(d$n_removed, 1L)
  expect_equal(d$n_added,   1L)
  expect_equal(d$removed,   "line2")
  expect_equal(d$added,     "lineX")
  expect_equal(d$n_unchanged, 2L)
})

test_that("compute_line_diff treats identical text as zero diff", {
  d <- compute_line_diff("a\nb\nc", "a\nb\nc")
  expect_equal(d$n_removed,   0L)
  expect_equal(d$n_added,     0L)
  expect_equal(d$n_unchanged, 3L)
})

test_that("compute_line_diff ignores blank/whitespace-only lines", {
  d <- compute_line_diff("a\n\n   \nb", "a\nb")
  expect_equal(d$n_removed,   0L)
  expect_equal(d$n_added,     0L)
  expect_equal(d$n_unchanged, 2L)
})

test_that("compute_line_diff is set-based (line moves are not changes)", {
  d <- compute_line_diff("a\nb\nc", "c\na\nb")
  expect_equal(d$n_removed, 0L)
  expect_equal(d$n_added,   0L)
  expect_equal(d$n_unchanged, 3L)
})

test_that("compute_line_diff dedup behaviour: setdiff treats lines as unique", {
  d <- compute_line_diff("a\na\nb", "a\nb")
  expect_equal(d$n_removed,   0L)
  expect_equal(d$n_added,     0L)
  expect_equal(d$n_unchanged, 2L)
})
