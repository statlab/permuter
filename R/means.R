#' One-sample permutation test for the center of a distribution
#'
#' @param x Vector of observations
#' @param reps Number of replications to approximate distribution (default 1000)
#' @param center Optional; specify null center of distribution (default 0)
#' 
#' @return A vector of length `reps` containing the permutation distribution
#' @examples
#' x <- -5:5 + rnorm(11)
#' distr <- one_sample(x)
#' pvalue <- t2p(mean(x), distr, alternative = "two-sided")
#' pvalue
#' 
#' distr2 <- one_sample(x, center = 5)
#' pvalue2 <- t2p(mean(x), distr2, alternative = "greater")
#' pvalue2
one_sample <- function(x, reps=1000, center=0){
  if(!is.vector(x)){ stop("x must be a vector") }
  if(!is.numeric(x)){ stop("x must be numeric") }
  
  x_center <- x - center
  n <- length(x)
  tst <- function(z){ mean(x_center*z + center)}
  distr <- replicate(reps, {
    perm <- 1-2*rbinom(n,1,.5)
    tst(perm)
  })
  return(distr)
}

#' Two-sample permutation test for equality of means
#'
#' @param x Vector of observations in group 1
#' @param y Vector of observations in group 2
#' @param reps Number of replications to approximate distribution (default 1000)
two_sample <- function(x, y, reps=1000){
  if(!is.vector(x) | !is.vector(y)){ stop("x and y must be vectors") }
  if(!is.numeric(x) | !is.numeric(y)){ stop("x and y must be numeric") }
  
  tst <- function(x,y) {mean(x) - mean(y)}
  z <- c(x,y)
  n <- length(x)
  distr <- replicate(reps, {
    z.star <- sample(z)
    x.star <- z.star[1:n]
    y.star <- z.star[-(1:n)]
    tst(x.star, y.star)
  })
  return(distr)
}
