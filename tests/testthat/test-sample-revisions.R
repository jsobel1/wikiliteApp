test_that("sample_revisions returns input unchanged when n >= nrow", {
  h <- data.frame(rev = 1:10, ts = 1:10)
  expect_identical(sample_revisions(h, 10), h)
  expect_identical(sample_revisions(h, 50), h)
})

test_that("sample_revisions(method='recent') keeps the last n rows in order", {
  h <- data.frame(rev = 1:100, ts = 1:100)
  out <- sample_revisions(h, 5, method = "recent")
  expect_equal(nrow(out), 5L)
  expect_equal(out$rev, 96:100)
})

test_that("sample_revisions(method='even') retains first and last and is monotone", {
  h <- data.frame(rev = 1:1000, ts = 1:1000)
  out <- sample_revisions(h, 20, method = "even")
  expect_lte(nrow(out), 20L)        # may be <= 20 due to unique() collapsing
  expect_gte(nrow(out), 18L)
  expect_equal(out$rev[1], 1L)
  expect_equal(out$rev[nrow(out)], 1000L)
  expect_true(all(diff(out$rev) > 0))
})

test_that("sample_revisions(method='random') retains first and last and is reproducible", {
  h <- data.frame(rev = 1:200, ts = 1:200)
  set.seed(42)
  out1 <- sample_revisions(h, 30, method = "random")
  set.seed(42)
  out2 <- sample_revisions(h, 30, method = "random")
  expect_identical(out1, out2)
  expect_lte(nrow(out1), 30L)
  expect_equal(out1$rev[1], 1L)
  expect_equal(out1$rev[nrow(out1)], 200L)
  expect_true(all(diff(out1$rev) > 0))
})

test_that("sample_revisions accepts non-integer n by rounding", {
  h <- data.frame(rev = 1:50)
  expect_equal(nrow(sample_revisions(h, 10.4, method = "recent")), 10L)
  expect_equal(nrow(sample_revisions(h, 10.6, method = "recent")), 11L)
})

test_that("sample_revisions floors at n=2", {
  h <- data.frame(rev = 1:10)
  out <- sample_revisions(h, 1, method = "recent")
  expect_equal(nrow(out), 2L)
})

test_that("sample_revisions returns input unchanged when n is invalid", {
  h <- data.frame(rev = 1:10)
  expect_identical(sample_revisions(h, NA), h)
  expect_identical(sample_revisions(h, NA_integer_), h)
})
