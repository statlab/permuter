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
t2p <- function(t, distr, alternative = c("greater", "less", "two-sided")) {
    
    # check that distr is a vector with appropriate size
    B <- sum(!is.na(distr))
    # check that t is just a single number
    
    pupper <- mean(distr >= t, na.rm = TRUE)
    plower <- mean(distr <= t, na.rm = TRUE)
    pboth <- mean(abs(distr) >= abs(t), na.rm = TRUE)
    
    # adjust so there are no 0 p-values? This is what their original code does.
    # pupper <- mean(distr >= t, na.rm=T) + 1/B 
    # plower <- mean(distr <= t, na.rm=T) + 1/B 
    # pboth <- mean(abs(distr) >= abs(t), na.rm=T) + 1/B
    
    P <- c(pupper = pupper, plower = plower, pboth = pboth)
    alt <- c("greater", "less", "two-sided")
    keep <- alt %in% alternative
    return(P[keep])
} 

#' Computes the p-value for every observation in an empirical distribution
#' 
#' @inheritParams t2p
pvalue_distr <- function(distr, alternative = "greater"){
  sapply(1:length(distr), function(x) t2p(distr[x], distr[-x], alternative))
}