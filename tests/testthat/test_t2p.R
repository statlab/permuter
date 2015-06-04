context("Bad inputs")

context("Correct p-values from t2p")
test_that("p-values from toy example", {
  expect_equal(t2p(5, -10:10, "greater"), c("pupper" = 6/21))
  expect_equal(t2p(5, -10:10, "less"), c("plower" = 16/21))
  expect_equal(t2p(5, -10:10, "two-sided"), c("pboth" = 12/21))
})

test_that("p-values for IPAT data",{
  data(ipat)
  d <- ipat$YA-ipat$YB
  distr <- one_sample(d)
  pval <- t2p(mean(d), distr)
  expect_equal(pval, c("pupper"=0,"plower"=1,"pboth"=0))
})