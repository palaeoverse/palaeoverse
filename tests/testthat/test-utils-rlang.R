test_that("check_class() handles columns with multiple classes", {
  x <- "a"
  class(x) <- c("foo", "bar", class(x))
  dat <- data.frame(x = x)

  expect_snapshot(
    check_class(dat, "x", "numeric"),
    error = TRUE
  )
})
