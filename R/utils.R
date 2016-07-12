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
    num_matrices <- length(x)
    x <- do.call(cbind, x)
  }
  
  rowcount <- nrow(x)
  perm <- sample(rowcount)
  xnew <- x[perm, ]
  if(split_cols){
    xnew <- lapply(1 + (-1 + seq_len(num_matrices))*split_cols, function(x) xnew[, x:(x+split_cols-1)])
  }
  return(xnew)
}

#' Fisher-Yates shuffle
#'
#' Also known as Knuth shuffle - a method of taking a random permutation of a list
#' 
#' @param x A vector to shuffle
#' @return The permuted data
fisher_yates <- function(x){
  n <- length(x)
  for(i in seq_along(x)){
    J <- i + floor(runif(1)*(n-i+1))
    x[c(i, J)] <- x[c(J, i)]
  }
  return(x)
}

#' Cormen et al. Random_Sample
#' 
#' Recursive method to take a simple random sample from a vector, which does not require sorting.
#' 
#' @param x A vector to from which to sample
#' @param k Desired sample size
#' @return The selected sample
Random_Sample <- function(x, k)
  if(k==0){
    return(c())
  } else{
    n <- length(x)
    S <- Random_Sample(x[1:(n-1)], k-1)
    i <- 1 + floor(runif(1)*n)
    if(x[i] %in% S){
      S <- c(S, x[n])
    } else {
      S <- c(S, x[i])
    }
    return(S)
  }