#' Description about the function
#' 
#' @param P something
#' @return something
FWE.minP_old <- function(P) {
    
    
    p.oss <- P[1, ]
    p.ord <- sort(p.oss, decreasing = FALSE)
    o <- order(p.oss, decreasing = FALSE)
    
    B <- dim(P)[1] - 1
    p <- dim(P)[2]
    
    p.ris <- array(0, dim = c(p, 1))
    
    
    P.ord <- P[, o]
    
    
    T <- apply(P.ord, 1, min)
    p.ris[1] <- mean(T[-1] <= T[1])
    
    if (p > 2) {
        for (j in 2:(p - 1)) {
            
            T <- apply(P.ord[, j:p], 1, min)
            p.ris[j] <- max(mean(T[-1] <= T[1]), p.ris[(j - 1)])
        }
    }
    p.ris[p] <- max(p.ord[p], p.ris[p - 1])
    
    p.ris[o] <- p.ris
    
    rownames(p.ris) <- colnames(P)
    
    return(p.ris)
} 

#' Permutation version of the Bonferroni-Holm minP step-up procedure
#' 
#' When considering a closed testing procedure, the adjusted p-value $p_i$ for a given hypothesis $H_i$ is the maximum of all p-values for tests including $H_i$ as a special case (including the p-value for the $H_i$ test itself).
#' 
#' @inheritParams npc
#' @return Vector of adjusted p-values
#' 
fwe_minp <- function(pvalues, distr, combine = "fisher") {
  
  # Choose the combining function, check inputs
  funcs <- list(fisher, liptak, tippett)
  names(funcs) <- c("fisher", "liptak", "tippett")
  p <- length(pvalues)
  if(!(combine %in% names(funcs))){ stop(paste(combine, " is not a valid combining function.")) }
  if(p < 2){ stop("Nothing to combine!") }
  if(ncol(distr) != p){ stop("Different number of p-values and null distributions")}
  combn_func <- funcs[[combine]]
  
  # Order the p-values
  p_ord <- sort(pvalues, decreasing = FALSE)
  perm_pvalues <- apply(distr, 2, pvalue_distr, alternative = "two-sided")
  perm_pvalues_ord <- perm_pvalues[ , order(p_raw)]
  
  # Step down tree of combined hypotheses, from global test to test of the 
  # individual hypothesis with largest p-value
  p_ris <- rep(NA, p)
  combined_stats <- apply(perm_pvalues_ord, 1, combn_func)
  obs_stat <- combn_func(p_ord)
  p_ris[1] <- t2p(obs_stat, combined_stats, alternative = "greater")
  if (p > 2) {
    for (j in 2:(p - 1)) {
      obs_stat <- combn_func(p_ord[j:p])
      combined_stats <- apply(perm_pvalues_ord[, j:p], 1, combn_func) 
      p_ris[j] = max(t2p(obs_stat, combined_stats, alternative = "greater"), p_ris[(j-1)])
    }
  }
  p_ris[p] <- max(p_ord[p], p_ris[p - 1])
  p_ris[order(p_raw)] <- p_ris
  return(p_ris)
} 

