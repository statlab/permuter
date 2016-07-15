set.seed(222)
pval <- seq(0.05, 0.9, length.out = 5)
distr <- matrix(runif(5 * 100, min = 0, max = 10), ncol = 5)

context("FWE MinP")
test_that("bad inputs", {
    expect_error(npc(pval, distr, combine = "something else"), "combining function")
    expect_error(npc(pval[1], distr[, 1]), "Nothing")
    expect_error(npc(pval, distr[, -1]), "Different number")
})

test_that("p-values from toy example", {
    expect_equal(fwe_minp(pval, distr, "fisher"), c(0.31, 0.73, 0.84, 0.9, 0.9))
    expect_equal(fwe_minp(pval, distr, "liptak"), c(0.39, 0.67, 0.77, 0.9, 0.9))
    expect_equal(fwe_minp(pval, distr, "tippett"), c(0.25, 0.68, 0.84, 0.89, 0.9))
})
