generate_test_case <- function(){
  R <- 10
  Ns <- 35
  set.seed(42)
  res <- matrix(rbinom(n = R * Ns, size = 1, p = 0.5), nrow = R, ncol = Ns)
  return(res)
}

context("IRR Test Statistic")
test_that("Test statistic", {
  simple <- matrix(c(1, 1, 1, 0, 0, 1, 0, 0), nrow = 2, byrow = TRUE)
  expected_simple <- 0.5
  expect_equal(expected_simple, compute_irr_ts(simple))
  
  res <- generate_test_case()
  rho_s <- compute_irr_ts(res)
  expect_equal(rho_s, 0.511746, tolerance = 1e-6)
})

test_that("Test statistic distribution", {
  simple <- matrix(c(1, 1, 1, 0, 0, 1, 0, 0), nrow = 2, byrow = TRUE)
  perm_distr <- irr_ts_distribution(simple, num_perm = 5, keep_dist = TRUE, seed = 5)
  expect_equal(length(perm_distr), 5)
  expect_is(perm_distr, "list")
  expect_equal(perm_distr$obs_ts, 0.5)
  expect_equal(perm_distr$geq, 4)
  expect_equal(perm_distr$num_perm, 5)
  expect_equal(perm_distr$pvalue, 4/5)
  expect_equal(perm_distr$dist, c(0.5, 0, 0.5, 0.5, 0.5))
  
  perm_distr2 <- irr_ts_distribution(simple, num_perm = 5, keep_dist = FALSE, seed = 5)
  expect_equal(length(perm_distr2), 5)
  expect_is(perm_distr2, "list")
  expect_equal(perm_distr2$obs_ts, 0.5)
  expect_equal(perm_distr2$geq, 4)
  expect_equal(perm_distr2$num_perm, 5)
  expect_equal(perm_distr2$pvalue, 4/5)
  expect_null(perm_distr2$dist)
  
  res <- generate_test_case()
  perm_distr <- irr_ts_distribution(res, num_perm = 5, keep_dist = TRUE, seed = 1)
  expect_equal(perm_distr$obs_ts, 0.511746, tolerance = 1e-6)
  expect_equal(perm_distr$geq, 0)
  expect_equal(perm_distr$num_perm, 5)
  expect_equal(perm_distr$pvalue, 0)
  expect_equal(perm_distr$dist, 
               c(0.4939683, 0.4761905, 0.4952381, 0.4901587, 0.4863492), 
               tolerance = 1e-6)
})