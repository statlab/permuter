#' Permute within groups
#' 
#' Permutation of condition within each group.
#' This is a helper function for stratified permutation tests.
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


#' Permute rows of a matrix
#' 
#' This is a helper function for multivariate permutation tests, when we must
#' permute multiple variables in lockstep in order to preserve correlations between
#' them and/or to apply NPC.
#' 
#' @param x A matrix or dataframe, or a list of matrices/dataframes of the same size
#' @return The permuted data. If x is a list, the returned list
#' will have the same permutation of rows in each matrix/dataframe.
#' 
permute_rows <- function(x){
  split_cols <- 0
  if(class(x) == "list"){
    split_cols <- ncol(x[[1]])
    x <- do.call(cbind, x)
  }
  
  rowcount <- nrow(x)
  perm <- sample(rowcount)
  xnew <- x[perm, ]
  if(split_cols){
    xnew <- lapply(1 + (-1 + seq_len(rowcount))*split_cols, function(x) xnew[, x:(x+split_cols-1)])
  }
  return(xnew)
}