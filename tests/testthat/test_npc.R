set.seed(55)

context("NPC")
test_that("bad inputs",{
  pval <- seq(0.05, 0.9, length.out = 5)
  distr <- matrix(runif(5*100, min=0, max=10), ncol = 5)
  expect_error(npc(pval, distr, combine = "something else"), "combining function") 
  expect_error(npc(pval[1], distr[,1]), "Nothing") 
  expect_error(npc(pval, distr[,-1]), "Different number")
  expect_error(npc(pval, distr, alternatives = c("greater","greater")), "Bad alternatives") 
})

test_that("p-values from toy example", {
  expect_equal(fisher(pval), 11.11546, tol = 1e-4)
  expect_equal(liptak(pval), 0.5728894, tol = 1e-6)
  expect_equal(tippett(pval), 0.95)
  expect_equal(npc(pval, distr, "fisher", "greater"), 0.38)
  expect_equal(npc(pval, distr, "fisher", "less"), 0.37)
  expect_equal(npc(pval, distr, "fisher", "two-sided"), 0.38)
  expect_equal(npc(pval, distr, "liptak"), 0.45)
  expect_equal(npc(pval, distr, "tippett"), 0.22)
  expect_equal(npc(pval, distr, alternatives = c("less","greater","less","greater","two-sided")), 0.37)
})