#' Compute the IRR test statistic
#'
#' Compute the test statistic \eqn{\rho_s}. See \href{http://statlab.github.io/permute/api/irr.html}{the Statlab documentation} for details.
#' @inheritParams irr_ts_distribution
#' @return concordance of the ratings, where perfect concordance is 1.0
compute_irr_ts <- function(ratings) {
    R <- nrow(ratings)
    Ns <- ncol(ratings)
    y <- apply(ratings, 2, sum)
    counts <- y * (y - 1) + (R - y) * (R - y - 1)
    rho_s <- sum(counts)/(Ns * R * (R - 1))
    return(rho_s)
} 



#' Get the permutation distribution of the IRR test statistic
#' 
#' Simulates the permutation distribution of the irr test statistic for
#' a matrix of ratings ``ratings``
#' 
#'         
#' If \code{obs_ts} is not \code{None}, computes the reference value of the test
#' statistic before the first permutation. Otherwise, uses the value
#' \code{obs_ts} for comparison.
#' 
#' If \code{keep_dist}, return the distribution of values of the test statistic;
#' otherwise, return only the number of permutations for which the value of
#' the irr test statistic is at least as large as \code{obs_ts}.
#' 
#' @param ratings matrix of dimension [R, Ns]. 
#'    Each row corresponds to the ratings given by a single rater; 
#'    columns correspond to items rated.
#' @param obs_ts if None, \code{obs_ts} is calculated as the value 
#'    of the test statistic for the original data
#' @param num_perm integer number of random permutations of the elements of each row of ratings
#' @param keep_dist Boolean flag for whether to store and return the array 
#'    of permutation values of the irr test statistic
#' @param seed Random seed for random number generator. 
#'    If NULL, the pseudorandom number generator is the instance used by the permutation function.
#' 
#' @return A list containing:
#' \itemize{
#' \item{obs_ts: observed value of the test statistic for the input data, or the input value of \code{obs_ts} if it was given as input}
#' \item{geq: integer number of iterations for which the test statistic was greater than or equal to \code{obs_ts}}
#' \item{num_perm: number of permutations}
#' \item{pvalue: geq/num_perm}
#' \item{dist: if \code{keep_dist}, the array of values of the irr test statistic from the \code{num_perm} iterations. Otherwise, NULL.}
#' }
irr_ts_distribution <- function(ratings, obs_ts = NULL, num_perm = 10000, keep_dist = FALSE,
                                seed = NULL){
  r = ratings
  if(!is.null(seed)){set.seed(seed)}
  
  if(is.null(obs_ts)){
    obs_ts <- compute_irr_ts(ratings)
  }
  
  if(keep_dist){
    dist <- rep(0, num_perm)
    for(i in seq_len(num_perm)){
      r <- permute_within_rows(ratings)
      dist[i] <- compute_irr_ts(r)
    }
    geq <- sum(dist >= obs_ts)
  } else {
    dist <- NULL
    geq <- 0
    for(i in seq_len(num_perm)){
      r <- permute_within_rows(ratings)
      geq <- geq + (compute_irr_ts(r) >= obs_ts)
    }
  }
  res <- list("obs_ts" = obs_ts, "geq" = geq, "num_perm" = num_perm,
              "pvalue" = (geq/num_perm), "dist" = dist)
  return(res)
}