set.seed(55)
pval <- seq(0.05, 0.9, length.out = 5)
tst <- c(9.6, 6.8, 5.8, 2.5, 1)
distr <- matrix(runif(5 * 100, min = 0, max = 10), ncol = 5)

context("NPC")
test_that("bad inputs", {
    expect_error(npc(tst, distr, combine = "something else"), "combining function")
    expect_error(npc(tst[1], distr[, 1]), "Nothing")
    expect_error(npc(tst, distr[, -1]), "Different number")
    expect_error(npc(tst, distr, alternatives = c("greater", "greater")), "Bad alternatives")
})

test_that("p-values from toy example", {
    expect_equal(npc(tst, distr, "fisher", "greater"), 0.27)
    expect_equal(npc(tst, distr, "fisher", "less"), 0.57)
    expect_equal(npc(tst, distr, "fisher", "two-sided"), 0.27)
    expect_equal(npc(tst, distr, "liptak"), 0.32)
    expect_equal(npc(tst, distr, "tippett"), 0.15)
    expect_equal(npc(tst, distr, combine = function(p) inverse_n_weight(p, rep(1, 
        length(tst)))), 0.43)
    expect_equal(npc(tst, distr, alternatives = c("less", "greater", "less", "greater", 
        "two-sided")), 0.62)
})

context("Combining functions")
test_that("Fisher combining function", {
    pval <- seq(0.05, 0.9, length.out = 5)
    expect_equal(fisher(pval), 11.11546, tolerance = 1e-05)
    expect_equal(fisher(1), 0)
    expect_lt(fisher(10), 0)
    expect_warning(fisher(-10), "NaN")
})

test_that("Liptak combining function", {
    pval <- seq(0.05, 0.9, length.out = 5)
    expect_equal(liptak(pval), 0.5728894, tolerance = 1e-05)
    expect_equal(liptak(1), -Inf)
    expect_warning(liptak(10), "NaN")
    expect_warning(liptak(-10), "NaN")
})

test_that("Tippett combining function", {
    pval <- seq(0.05, 0.9, length.out = 5)
    expect_equal(tippett(pval), 0.95)
    expect_equal(tippett(1), 0)
    expect_equal(tippett(10), -9)
})

test_that("Inverse n weight combining function", {
    pval <- seq(0.05, 0.9, length.out = 5)
    expect_equal(inverse_n_weight(pval, rep(1, 5)), -1 * sum(pval))
    expect_equal(inverse_n_weight(pval, rep(4, 5)), -0.5 * sum(pval))
    expect_equal(inverse_n_weight(pval, c(4, 9, 16, 25, 36)), -sum(1/(2:6) * pval))
    expect_error(inverse_n_weight(pval, (1:5) + 0.2))
    expect_error(inverse_n_weight(pval, 1:3))
})
