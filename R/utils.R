#' Permute within groups
#' 
#' Permutation of condition within each group.
#' 
#' @param x A vector of treatment indicators
#' @param group A vector indicating group membership
#' 
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