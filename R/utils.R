#' Permute within groups
#' 
#' Permutation of condition within each group.
#' 
#' @param x A vector of treatment indicators
#' @param group A vector indicating group membership
#' @return The within-group permuted x
permute_within_groups <- function(x, group){
  for(g in unique(group)){
    gg <- (group == g)
    x[gg] <- sample(x[gg])
  }
  return(x)
}


#' Permute a vector
#' 
#' @param x A vector
#' @return the permuted vector
permute <- function(x){
  return(sample(x))
}


#' Permute values within rows of a matrix
#' 
#' @param x A matrix or dataframe
#' @return The matrix with its rows permuted
permute_within_rows <- function(x){
  for(row in seq_len(nrow(x))){
    x[row,] <- sample(x[row,])
  }
  return(x)
}


#' Difference in means within groups
#'
#' Compute difference in mean residual between treated and control within each group
#' @param group Vector of group memberships or treatment conditions
#' @param response Vector of measured outcomes, same length as group
#' @param stratum Vector of stratum assignments, same length as group
#' @param groups Vector of unique group labels
#' @param strata Vector of unique stratum labels
#' 
#' @return a vector of differences
within_group_mean <- function(group, response, stratum, groups, strata){
  tt <- (group == groups[1])
  sapply(strata, function(s){
    ind <- stratum == s
    treated <- response[tt == 1 & ind]
    ctrl <- response[tt == 0 & ind]
    return(mean(treated, na.rm=TRUE) - mean(ctrl, na.rm=TRUE))
  })
}
