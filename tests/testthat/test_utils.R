context("permute functions")
test_that("simple permute", {
  simple <- c(1, 1, 1, 0, 0, 1, 0, 0)
  set.seed(42)
  simple_permuted <- permute(simple)
  expect_equal(simple_permuted, c(0,0,1,0,1,1,0,1))
})

test_that("permute within groups", {
  simple <- c(1, 1, 1, 0, 0, 1, 0, 0)
  groups <- c(1, 1, 2, 2, 2, 2, 3, 3)
  set.seed(42)
  simple_gp_permuted <- permute_within_groups(simple, groups)
  expect_equal(simple_gp_permuted, c(1, 1, 0, 0, 1, 1, 0, 0))
})

test_that("permute within rows", {
  simple <- matrix(c(1, 1, 1, 0, 0, 1, 0, 0), nrow = 4, byrow = TRUE)
  set.seed(42)
  simple_row_perm <- permute_within_rows(simple)
  expect_equal(simple[1,], simple_row_perm[1,])
  expect_equal(simple[2,], simple_row_perm[2,])
  expect_equal(simple[3,], simple_row_perm[3,2:1])
  expect_equal(simple[4,], simple_row_perm[4,])
})

context("test stat functions")
test_that("within group means", {
  group <- c(rep(1, 4), rep(2, 4))
  stratum <- c("b", rep("a", 4), rep("b", 3))
  response <- 1:8
  groups <- unique(group)
  strata <- unique(stratum)
  
  diff_strat1 <- 1 - mean(6:8)
  diff_strat2 <- mean(2:4) - 5
  expected <- c("b" = diff_strat1, "a" = diff_strat2)
  res <- within_group_mean(group, response, stratum, groups, strata)
  expect_equal(expected, res)
})