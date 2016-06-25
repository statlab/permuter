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
#' If \code{obs_ts} is not null, computes the reference value of the test
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
#' @param reps integer number of random permutations of the elements of each row of ratings
#' @param keep_dist Boolean flag for whether to store and return the array 
#'    of permutation values of the irr test statistic
#' @param seed Random seed for random number generator. 
#'    If NULL, the pseudorandom number generator is the instance used by the permutation function.
#' 
#' @return A list containing:
#' \itemize{
#' \item{obs_ts: observed value of the test statistic for the input data, or the input value of \code{obs_ts} if it was given as input}
#' \item{geq: integer number of iterations for which the test statistic was greater than or equal to \code{obs_ts}}
#' \item{reps: number of permutations}
#' \item{pvalue: geq/reps}
#' \item{dist: if \code{keep_dist}, the array of values of the irr test statistic from the \code{reps} iterations. Otherwise, NULL.}
#' }
irr_ts_distribution <- function(ratings, obs_ts = NULL, reps = 10000, keep_dist = FALSE,
                                seed = NULL){
  r = ratings
  if(!is.null(seed)){set.seed(seed)}
  
  if(is.null(obs_ts)){
    obs_ts <- compute_irr_ts(ratings)
  }
  
  if(keep_dist){
    dist <- rep(0, reps)
    for(i in seq_len(reps)){
      r <- permute_within_rows(ratings)
      dist[i] <- compute_irr_ts(r)
    }
    geq <- sum(dist >= obs_ts)
  } else {
    dist <- NULL
    geq <- 0
    for(i in seq_len(reps)){
      r <- permute_within_rows(ratings)
      geq <- geq + (compute_irr_ts(r) >= obs_ts)
    }
  }
  res <- list("obs_ts" = obs_ts, "geq" = geq, "reps" = reps,
              "pvalue" = (geq/reps), "dist" = dist)
  return(res)
}


#' Combine the IRR statistics across strata
#' 
#' Use NPC to combine the IRR statistics across strata.
#' 
#'     Simulates the permutation distribution of the combined NPC test
#'     statistic for S matrices of ratings \code{obs_ts} corresponding to
#'     S strata. The distribution comes from applying \code{obs_ts}
#'     to each of the S strata.
#'     
#'     If \code{obs_ts} is not null, computes the reference value of the test
#'     statistic before the first permutation. Otherwise, uses the value
#'     \code{obs_ts} for comparison.
#'     
#'     If \code{keep_dist}, return the distribution of values of the test
#'     statistic; otherwise, return only the number of permutations
#'     for which the value of the irr test statistic is at least
#'     as large as \code{obs_ts}.
#'     
#' @param perm_distr Input matrix of dimension [B, S]
#'        Column s is the permutation distribution of \eqn{rho_s}, for \eqn{s = 1, \dots, S}.
#' @param size Vector of S sample sizes. 
#'        Entry s is the number of items \eqn{N_s} in stratum s.
#' @param obs_ts Optional: the NPC test statistic.
#'        If NULL, the NPC test statistic is computed using \code{pvalues}.
#' @param pvalues Optional: a vector of S IRR p-values for each stratum.
#'        Entry s is the p-value for \eqn{rho_s}, the concordance in stratum s.
#' 
#' @return A list containing:
#' \itemize{
#' \item{obs_npc: observed value of the test statistic for the input data, or the input value of \code{obs_ts} if it was given as input}
#' \item{geq: integer number of iterations for which the test statistic was greater than or equal to \code{obs_ts}}
#' \item{reps: number of permutations}
#' \item{pvalue: geq/reps}
#' \item{dist: if \code{keep_dist}, the array of values of the irr test statistic from the \code{reps} iterations. Otherwise, NULL.}
#' }
#' 
irr_npc_distribution <- function(perm_distr, size, obs_ts = NULL, pvalues = NULL){
  if(is.null(obs_ts) & is.null(pvalues)){
    stop("You must input either obs_ts or pvalues")
  }
  
  B <- nrow(perm_distr)
  S <- ncol(perm_distr)
  combine_func <- function(p){inverse_n_weight(p, size)}
  
  if(is.null(pvalues)){
    pvalues <- rep(NA, S)
    for(j in seq_len(S)){
      pvalues[j] <- mean(perm_distr[,j] >= obs_ts[j])
    }
  }
  
  obs_npc <- combine_func(pvalues)
  res <- npc(pvalues, perm_distr, combine_func, alternatives = "greater")
  return(list("obs_npc" = obs_npc,
              "pvalue"  = res,
              "reps"= B))
} 