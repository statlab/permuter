context("Bad inputs")

context("Correct p-values from t2p")
test_that("p-values from toy example", {
    expect_equal(t2p(5, -10:10, "greater"), c(Upper = 6/21))
    expect_equal(t2p(5, -10:10, "less"), c(Lower = 16/21))
    expect_equal(t2p(5, -10:10, "two-sided"), c(`Two-sided` = 12/21))
})

test_that("p-values for IPAT data", {
    data(ipat)
    set.seed(5)
    d <- ipat$YA - ipat$YB
    distr <- one_sample(d)
    pval <- t2p(mean(d), distr)
    expect_equal(pval, c(Upper = 0, Lower = 1, `Two-sided` = 0))
})

context("Correct p-values from pvalue_distr")
test_that("p-values from toy example", {
    d <- 1:5
    res <- pvalue_distr(d)
    names(res) <- NULL
    expect_equal(res, seq(1, 0.2, by = -0.2))
    res <- pvalue_distr(d, alternative = "two-sided")
    names(res) <- NULL
    expect_equal(res, c(0.4, 0.8, 1, 0.8, 0.4))
    res <- pvalue_distr(d, alternative = "less")
    names(res) <- NULL
    expect_equal(res, seq(0.2, 1, by = 0.2))
})
