#' Permute within groups
#'
#' Permute values within groups
#' @param x     vector of exchangeable values
#' @param group vector of fixed group assignments
#' @return a vector of length(x)
permute_within_groups <- function(x, group){
  groups <- unique(group)
  permuted <- x
  for(gg in groups){
    permuted[group == gg] <- sample(x[group == gg])
  }
  return(permuted)
}



#' k-sample permutation test for equality of distributions
#'
#' @inheritParams one_sample
#' @param group  Vector of group memberships, exchangeable under the null
#' @param strata Vector of secondary group memberships (optional); if supplied, 
#' permutations will be done within strata
#' @param stat  Test statistic: either "oneway_anova" (default) or "twoway_anova"
#' @return A vector of length `reps` containing the permutation distribution
k_sample <- function(x, group, strata=NULL, reps=1000, stat = "oneway_anova"){
  n <- table(group)
  
  ## check that if twoway_anova is specified, then group2 != NULL
  
  oneway_anova <- function(group){
    k <- unique(group)
    xbar <- rep(NA, length(k))
    for(gg in k){
      xbar[gg] <- mean(x[group == gg])
    }
    tst <- sum(xbar^2*n) # The permutation ANOVA statistic: sum_{j in groups} n_j*(mean(X_j))^2
    return(tst)
  }
  twoway_anova <- function(group){
    k <- unique(group)
    xbar <- rep(NA, length(k))
    for(gg in 1:length(k)){
      xbar[gg] <- mean(x[group == k[gg]])
    }
    xbarbar <- mean(xbar)
    tst <- sum((xbar - xbarbar)^2) # The permutation two-way ANOVA stat is SSB=sum_{j in groups} (mean(X_j) - grand mean)^2
    return(tst)
  }
  test_stats <- list(oneway_anova, twoway_anova)
  names(test_stats) <- c("oneway_anova", "twoway_anova")
  tst <- test_stats[[stat]]
  
  if(is.null(strata)){
    perm_method <- function(g) sample(g)
  }else{
    perm_method <- function(g) permute_within_groups(g, strata)
  }
  
  distr <- replicate(reps, {
              perm <- perm_method(group)
              tst(perm)
            })
  return(distr)
}

#TODO -- constrained permutation
#' k-sample test for equality of distributions
#'
#' @inheritParams k_sample
#' @param stat  either "ks" (Kolmogorov-Smirnov, default) or "ad" (Anderson-Darling)
#' 
#' @return A vector of length `reps` containing the permutation distribution
goodness_of_fit <- function(x, group, reps=1000, stat = "ks"){
  return(NULL)
}
