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
