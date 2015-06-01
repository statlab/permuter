context("Bad inputs")

context("Correct p-values")
test_that("p-values are correct", {
  expect_equal(t2p(5, -10:10, "upper"), c("pupper" = 6/21))
  expect_equal(t2p(5, -10:10, "lower"), c("plower" = 16/21))
  expect_equal(t2p(5, -10:10, "two-sided"), c("pboth" = 12/21))
})