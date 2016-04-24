R <- 10
Ns <- 35

set.seed(42)
res <- matrix(rbinom(n = R * Ns, size = 1, p = 0.5), nrow = R, ncol = Ns)

context("IRR Test Statistic")
test_that("Test statistic", {
  simple <- matrix(c(1, 1, 1, 0, 0, 1, 0, 0), nrow = 2, byrow = TRUE)
  expected_simple <- 0.5
  expect_equal(expected_simple, compute_irr_ts(simple))

  rho_s <- compute_irr_ts(res)
  expect_equal(rho_s, 0.511746, tolerance = 1e-6)
})