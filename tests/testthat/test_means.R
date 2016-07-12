set.seed(55)

context("One sample")
test_that("bad inputs", {
    expect_error(one_sample("k"), "numeric")
    expect_error(one_sample(data.frame(1:5)), "vector")
})

test_that("IPAT example from chapter 1", {
    data(ipat)
    d <- ipat$YA - ipat$YB
    distr <- one_sample(d)
    expect_equal(min(distr), -2.6)
    expect_equal(max(distr), 2.6)
    expect_equal(median(distr), 0)
})

context("Two sample")
test_that("bad inputs", {
    expect_error(two_sample("k", 4), "numeric")
    expect_error(two_sample(data.frame(1:5), 40), "vector")
})

test_that("jobs example from chapter 1", {
    data(job)
    group1 <- job[job$Y == 1, "X"]
    group2 <- job[job$Y == 2, "X"]
    observed <- mean(group1) - mean(group2)
    distr <- two_sample(group1, group2)
    expect_equal(min(distr), -16.875)
    expect_equal(max(distr), 18 + 1/3)
    expect_equal(median(distr), 0)
}) 

context("Stratified two sample")
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


test_that("bad inputs", {
  expect_error(stratified_two_sample(4, "k", 5), "numeric")
  expect_error(stratified_two_sample(data.frame(1:5), 20, 40), "vector")
  expect_error(stratified_two_sample(5, 20, 40, stat="lame"), "dictionary of stats")
})

test_that("simple example", {
  group <- c(rep(1, 4), rep(2, 4))
  stratum <- c("b", rep("a", 4), rep("b", 3))
  response <- 1:8
  
  set.seed(55)
  distr <- stratified_two_sample(group, response, stratum)
  expect_equal(min(distr), -4.0)
  expect_equal(median(distr), 0)
  expect_equal(max(distr), 4.0)
  
  set.seed(55)
  distr <- stratified_two_sample(group, response, stratum, stat = "mean_within_strata")
  expect_equal(min(distr), 1.333, tolerance = 1e-3)
  expect_equal(median(distr), 4.0)
  expect_equal(max(distr), 9.333, tolerance = 1e-3)
})

test_that("example from permute",{
  stratum <- rep(1:3, each=10)
  group <- rep( rep(1:2, each=5), 3)
  response <- rep(0, length(group))
  response[c(1, 2, 4, 10, 11, 12, 19, 20, 21)] <- 1
  
  set.seed(55)
  res <- stratified_two_sample(group, response, stratum, reps=10000)
  expect_equal(mean(response[group==1]) - mean(response[group==2]), 0.2)
  expect_equal(c("Upper"=0.2169), t2p(0.2, res, "greater"))
})