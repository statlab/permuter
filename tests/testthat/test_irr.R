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
  expected_perm_distr <- list("obs_ts" = 0.5,
                              "geq" = 4,
                              "num_perm" = 5,
                              "pvalue" = 4/5,
                              "dist" = c(0.5, 0, 0.5, 0.5, 0.5))
  expect_equal(perm_distr, expected_perm_distr)
  
  perm_distr <- irr_ts_distribution(simple, num_perm = 5, keep_dist = FALSE, seed = 5)
  expected_perm_distr <- list("obs_ts" = 0.5,
                              "geq" = 4,
                              "num_perm" = 5,
                              "pvalue" = 4/5,
                              "dist" = NULL)
  expect_equal(perm_distr, expected_perm_distr)
  
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

context("IRR plus NPC")
test_that("NPC test stat distribution", {
  res <- generate_test_case()
  perm_distr1 <- irr_ts_distribution(res, num_perm = 100, keep_dist = TRUE, seed = 1)
  perm_distr2 <- irr_ts_distribution(res, num_perm = 100, keep_dist = TRUE, seed = 3)
  perm_distr3 <- irr_ts_distribution(res, num_perm = 100, keep_dist = TRUE, seed = 5)
  perm_distr_list <- list(perm_distr1, perm_distr2, perm_distr3)
  
  sidebyside <- sapply(perm_distr_list, function(x) x$dist)
  pvalues <- sapply(perm_distr_list, function(x) x$pvalue)
  obs_ts <- sapply(perm_distr_list, function(x) x$obs_ts)
  res1 <- irr_npc_distribution(sidebyside, size = rep(35, 3), obs_ts = obs_ts)
  res2 <- irr_npc_distribution(sidebyside, size = rep(35, 3), pvalues = pvalues)
  expected_res <- list("obs_npc" = -0.07606388,
                       "pvalue" = 0.02,
                       "num_perm" = 100)
  
  expect_equal(expected_res, res1, tol = 1e-6)
  expect_equal(expected_res, res2, tol = 1e-6)  
  
  # Perfect concordance between raters in two strata
  B <- 100
  mat1 <- matrix(rep(c(1, 0, 1, 0, 0), 5), nrow = 5, byrow = TRUE)
  mat2 <- matrix(rep(c(0, 1, 0), 5), nrow = 5, byrow = TRUE)
  strata <- list(mat1, mat2)
  size <- c(nrow(mat1), nrow(mat2))
  distributions <- matrix(NA, ncol = 2, nrow = B)
  tst <- rep(NA, 2)
  pval <- rep(NA, 2)
  for(j in seq_along(strata)){
    res <- irr_ts_distribution(strata[[j]], keep_dist = TRUE, num_perm = B, seed = 5)
    distributions[,j] <- res$dist
    tst[j] <- res$obs_ts
    pval[j] <- res$pvalue
  }
  npc_res1 <- irr_npc_distribution(distributions, size = size, obs_ts = tst)
  npc_res2 <- irr_npc_distribution(distributions, size = size, pvalues = pval)
  expected_npc_res <- list("obs_npc" = 0,
                           "pvalue" = 0,
                           "num_perm" = B)
  expect_equal(npc_res1, expected_npc_res)  
  expect_equal(npc_res2, expected_npc_res)
})