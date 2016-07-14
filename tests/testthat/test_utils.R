context("permute functions")
test_that("simple permute", {
    simple <- c(1, 1, 1, 0, 0, 1, 0, 0)
    set.seed(42)
    simple_permuted <- permute(simple)
    expect_equal(simple_permuted, c(0, 0, 1, 0, 1, 1, 0, 1))
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
    expect_equal(simple[1, ], simple_row_perm[1, ])
    expect_equal(simple[2, ], simple_row_perm[2, ])
    expect_equal(simple[3, ], simple_row_perm[3, 2:1])
    expect_equal(simple[4, ], simple_row_perm[4, ])
})

test_that("permute rows - dataframe version", {
    simple <- matrix(c(1, 1, 1, 0, 0, 1, 0, 0), nrow = 4, byrow = TRUE)
    set.seed(42)
    res <- permute_rows(simple)
    expected <- matrix(c(0, 0, 0, 1, 1, 1, 1, 0), nrow = 4, byrow = TRUE)
    expect_equal(res, expected)
})

test_that("permute rows - list version", {
    simple1 <- matrix(c(1, 1, 1, 0, 0, 1, 0, 0), nrow = 4, byrow = TRUE)
    simple2 <- matrix(c(1, 1, 1, 0, 0, 1, 0, 0), nrow = 4, byrow = TRUE)
    simple_list <- list(simple1, simple2)
    
    set.seed(42)
    res <- permute_rows(simple_list)
    expect_equal(res[[1]], res[[2]])
    expected <- matrix(c(0, 0, 0, 1, 1, 1, 1, 0), nrow = 4, byrow = TRUE)
    expect_equal(res[[1]], expected)
})

test_that("Fisher-Yates shuffle", {
    set.seed(42)
    res <- fisher_yates(1:5)
    expected <- c(5, 1, 3, 2, 4)
    expect_equal(res, expected)
})

test_that("RCpp permute", {
    # Since it uses the R PRNG and Fisher-Yates algo it should give the same output
    # as Fisher-Yates
    x <- 1:10
    set.seed(5)
    expected <- fisher_yates(x)
    set.seed(5)
    res <- permute_cpp(x)
    expect_equal(res, expected)
})

context("Random sampling")

test_that("Cormen et al. Random_Sample", {
    set.seed(42)
    res <- Random_Sample(1:20, k = 5)
    expected <- c(15, 16, 6, 19, 13)
    expect_equal(res, expected)
    
    the_list <- sample(100)[1:20]
    set.seed(42)
    res <- Random_Sample(the_list, k = 5)
    expect_equal(res, the_list[expected])
})
