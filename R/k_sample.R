#' k-sample permutation test for equality of distributions
#'
#' @inheritParams one_sample
#' @param group Vector of group memberships
#' 
#' @return A vector of length `reps` containing the permutation distribution
k_sample <- function(x, group, reps=1000){
  n <- table(group)
  anova <- function(group){
    k <- unique(group)
    xbar <- rep(NA, length(k))
    for(gg in k){
      xbar[gg] <- mean(X[group == gg], na.rm=T)
    }
    tst <- sum(xbar^2*n) # The permutation ANOVA statistic: sum_{groups} n_j*(mean(X_j))^2
    return(tst)
  }
  
  distr <- replicate(reps, {
              perm <- sample(group)
              anova(perm)
            })
  return(distr)
}