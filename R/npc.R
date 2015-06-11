#' Combining functions
#' 
#' Fisher combining function is \eqn{-2\sum log(p)}.
#' @param p Vector of p-values
#' 
fisher <- function(p){-2*log(prod(p))}

#' @describeIn fisher
#' \eqn{\sum \Phi^{-1}(1-p)}.
liptak <- function(p){sum(qnorm(1-p))}

#' @describeIn fisher
#' \eqn{max(1-p)}.
tippett <- function(p){max(1-p)}


#' Non-parametric combination of tests
#'
#' Combines partial p-values from individual hypothesis tests \eqn{H_{0i}} against \eqn{H_{1i}}
#' to test the global null hypothesis
#' \deqn{\cap_{i} H_{0i}}
#' against the alternative
#' \deqn{\cup_{i} H_{1i}}
#' using a combining function.
#'
#' For details on the combining functions, see \code{\link{fisher}}, \code{\link{liptak}}, and \code{\link{tippett}}.
#' 
#' Alternative options are "greater", "less", or "two-sided". If specified, length of alternatives must
#' either be 1 or match the length of p.
#' @param pvalues       Vector of partial p-values for tests
#' @param distr         Matrix or dataframe, columns are approimate null distribution for each partial test
#' @param combine       Combining function (default is Fisher)
#' @param alternatives  Optional, vector of alternatives for each test (default is all "greater")
#' 
#' 
#' @return A single p-value for the global test
#' 
npc <- function(pvalues, distr, combine = "fisher", alternatives = "greater"){

  funcs <- list(fisher, liptak, tippett)
  names(funcs) <- c("fisher", "liptak", "tippett")
  if(!(combine %in% names(funcs))){ stop(paste(combine, " is not a valid combining function.")) }
  if(length(pvalues) < 2){ stop("Nothing to combine!") }
  if(length(pvalues) != ncol(distr)){ stop("Different number of p-values and null distributions")}
  if(length(alternatives)!=length(pvalues)){ 
    if(length(alternatives)==1){
      alternatives <- rep(alternatives, length(pvalues))
    }else{
      stop("Bad alternatives")
    }
  }
  combn_func <- funcs[[combine]]
  
  
  null_pvalues <- sapply(1:ncol(distr), function(j){
    sapply(1:nrow(distr), function(b) t2p(distr[b,j], distr[-b,j], alternatives[j]))
  })
  
  combined_pvalues <- apply(null_pvalues, 1, combn_func)
  obs_combined_pvalue <- combn_func(pvalues)
  return(mean(combined_pvalues >= obs_combined_pvalue))
}