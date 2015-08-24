set.seed(55)
pval <- seq(0.05, 0.9, length.out = 5)
distr <- matrix(runif(5 * 100, min = 0, max = 10), ncol = 5)

context("NPC")
test_that("bad inputs", {
    expect_error(npc(pval, distr, combine = "something else"), "combining function")
    expect_error(npc(pval[1], distr[, 1]), "Nothing")
    expect_error(npc(pval, distr[, -1]), "Different number")
    expect_error(npc(pval, distr, alternatives = c("greater", "greater")), "Bad alternatives")
})

test_that("p-values from toy example", {
    expect_equal(npc(pval, distr, "fisher", "greater"), 0.34)
    expect_equal(npc(pval, distr, "fisher", "less"), 0.35)
    expect_equal(npc(pval, distr, "fisher", "two-sided"), 0.34)
    expect_equal(npc(pval, distr, "liptak"), 0.35)
    expect_equal(npc(pval, distr, "tippett"), 0.25)
    expect_equal(npc(pval, distr, alternatives = c("less", "greater", "less", "greater", 
        "two-sided")), 0.4)
})

context("Fisher")
test_that("Fisher combining function", {
    pval <- seq(0.05, 0.9, length.out = 5)
    expect_equal(fisher(pval), 11.11546, tolerance = 1e-05)
    expect_equal(fisher(1), 0)
    expect_less_than(fisher(10), 0)
    expect_warning(fisher(-10), "NaN")
})

context("Liptak")
test_that("Liptak combining function", {
    pval <- seq(0.05, 0.9, length.out = 5)
    expect_equal(liptak(pval), 0.5728894, tolerance = 1e-05)
    expect_equal(liptak(1), -Inf)
    expect_warning(liptak(10), "NaN")
    expect_warning(liptak(-10), "NaN")
})

context("Tippett")
test_that("Tippett combining function", {
    pval <- seq(0.05, 0.9, length.out = 5)
    expect_equal(tippett(pval), 0.95)
    expect_equal(tippett(1), 0)
    expect_equal(tippett(10), -9)
}) 
