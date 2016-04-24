#' Compute the IRR test statistic
#'
#' Compute the test statistic \eqn{\rho_s}. See \href{http://statlab.github.io/permute/api/irr.html}{the Statlab documentation} for details.
#' @param ratings     matrix of dimension [R, Ns]. Each row corresponds to the ratings given by a single rater; columns correspond to items rated.
#' @return concordance of the ratings, where perfect concordance is 1.0
compute_irr_ts <- function(ratings) {
    R <- nrow(ratings)
    Ns <- ncol(ratings)
    y <- rowsum(ratings)
    counts <- y * (y - 1) + (R - y) * (R - y - 1)
    rho_s <- sum(counts)/(Ns * R * (R - 1))
    return(rho_s)
} 
