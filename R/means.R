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
#' pvalue <- t2p(mean(x), distr, alternative = 'two-sided')
#' pvalue
#' 
#' distr2 <- one_sample(x, center = 5)
#' pvalue2 <- t2p(mean(x), distr2, alternative = 'greater')
#' pvalue2
one_sample <- function(x, reps = 1000, center = 0) {
    if (!is.vector(x)) {
        stop("x must be a vector")
    }
    if (!is.numeric(x)) {
        stop("x must be numeric")
    }
    
    x_center <- x - center
    n <- length(x)
    tst <- function(z) {
        mean(x_center * z + center)
    }
    distr <- replicate(reps, {
        perm <- 1 - 2 * rbinom(n, 1, 0.5)
        tst(perm)
    })
    return(distr)
}

#' Two-sample permutation test for equality of means
#'
#' @param x Vector of observations in group 1
#' @param y Vector of observations in group 2
#' @param reps Number of replications to approximate distribution (default 1000)
two_sample <- function(x, y, reps = 1000) {
    if (!is.vector(x) | !is.vector(y)) {
        stop("x and y must be vectors")
    }
    if (!is.numeric(x) | !is.numeric(y)) {
        stop("x and y must be numeric")
    }
    
    tst <- function(x, y) {
        mean(x) - mean(y)
    }
    z <- c(x, y)
    n <- length(x)
    distr <- replicate(reps, {
        z.star <- sample(z)
        x.star <- z.star[1:n]
        y.star <- z.star[-(1:n)]
        tst(x.star, y.star)
    })
    return(distr)
}



#' Confidence interval for the additive shift in means for a one- or two-sample problem
#'
#' Invert the permutation test to get a \eqn{1-\alpha} confidence interval for either the shift parameter in a two-sample problem or the center of the distribution in a one-sample problem
#' @inheritParams two_sample
#' @param side Type of interval, either 'both', 'upper', or 'lower'. Default is 'both'.
#' @param alpha Significance level
#' @return a vector containing the desired limits of the confidence set
CI_mean <- function(x, y = NULL, reps = 1000, side = "both", alpha = 0.05) {
    
    if (!(side %in% c("upper", "lower", "both"))) {
        stop("side must be one of 'upper', 'lower', or 'both'")
    }
    if (side == "both") {
        d1 <- CI_mean(x = x, y = y, reps = reps, side = "lower", alpha/2)
        d2 <- CI_mean(x = x, y = y, reps = reps, side = "upper", alpha/2)
        return(c(d1, d2))
    }
    alternative <- ifelse(side == "upper", "less", "greater")
    
    # initialize
    
    shift_pval <- 1
    if (is.null(y)) {
        observed <- mean(x)
        shift_permtest <- function(ss) {
            shift_fun <- function(shift) one_sample(x, reps = reps, center = shift)
            distr <- shift_fun(ss)
            return(t2p(observed, shift_fun(ss), alternative = alternative))
        }
    } else {
        observed <- mean(x) - mean(y)
        shift_permtest <- function(ss) {
            shift_fun <- function(shift) two_sample(x - shift, y, rep = reps)
            distr <- shift_fun(ss)
            return(t2p(observed - ss, shift_fun(ss), alternative = alternative))
        }
    }
    d_prev <- observed
    d_next <- observed
    incr <- diff(range(c(x, y)))/2
    
    # Conduct permutation test for H0: shift = d until we reject H0
    while (shift_pval > alpha) {
        d_prev <- d_next
        d_next <- ifelse(side == "upper", d_prev + incr, d_prev - incr)
        shift_pval <- shift_permtest(d_next)
    }
    
    # Bisection
    if (side == "upper") {
        l_int <- d_prev
        u_int <- d_next
    } else {
        l_int <- d_next
        u_int <- d_prev
    }
    d <- uniroot(function(z) {
        shift_permtest(z) - alpha
    }, lower = l_int, upper = u_int, tol = alpha/reps)
    return(d$root)
} 
