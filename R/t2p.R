#' Use the empirical distribution of a test statistic to get p-values.
#' 
#' @param T an array/vector with the observed test statistic in the first entry and the empirical distribution in the remaining entries
#' @return An array of dimension (B+1)\times `dim(T)`. The first element (of the first dimension) contains the p-value
t2p_old <- function(T) {
    
    if (is.null(dim(T))) {
        T <- array(T, dim = c(length(T), 1))
    }
    oth <- seq(1:length(dim(T)))[-1]
    
    B <- dim(T)[1] - 1
    p <- dim(T)[2]
    if (length(dim(T)) == 3) {
        C <- dim(T)[3]
    }
    
    
    rango <- function(x) {
        # rango is the Italian word for rank
        r <- 1 - rank(x[-1], ties.method = "min")/B + 1/B
        return(c(mean(x[-1] >= x[1]), r))
    }
    
    P <- apply(T, oth, rango)
    return(P)
}

#' Use the empirical distribution of a test statistic to get p-values.
#' 
#' @param t           The observed test statistic
#' @param distr       The empirical distribution, computed by Monte Carlo
#' @param alternative P-value desired: 'greater', 'less', 'two-sided' (may specify more than one)
#' @return the p-value(s)
t2p <- function(tst, distr, alternative = c("greater", "less", "two-sided")) {
    
    # check that distr is a vector with appropriate size
    B <- sum(!is.na(distr))
    
    # check that t is just a single number
    
    p <- c()
#    pupper <- c("Upper" = mean(distr >= tst, na.rm = TRUE))
    pupper <- c("Upper" = mean(distr >= tst, na.rm = TRUE))
    plower <- c("Lower" =  mean(distr <= tst, na.rm = TRUE))
    if("greater" %in% alternative){
      p <- c(p, pupper)
    }
    if("less" %in% alternative){
      p <- c(p, plower)
    }
    if("two-sided" %in% alternative){
      pboth <- c("Two-sided" = 2*min(c(pupper, plower)))
      p <- c(p, pboth)
    }
    return(p)
}

#' Computes the p-value for every observation in an empirical distribution
#' 
#' @inheritParams t2p
pvalue_distr <- function(distr, alternative = "greater") {
  B <- length(distr)
  if(alternative == "less"){
      return(rank(distr, ties.method = "max")/B)
    } else{
      pupper <- rank(-distr, ties.method = "max")/B
      if(alternative == "greater"){
        return(pupper)
      }else{
        plower <- rank(distr, ties.method = "max")/B
        return(pmin(2*pmin(pupper, plower), 1))
      }
    }
}


#' Computes the p-value for every observation in an empirical distribution
#' 
#' @inheritParams t2p
pvalue_distr_old <- function(distr, alternative="greater"){
  sapply(1:length(distr), function(x) t2p(distr[x], distr, alternative))
}
