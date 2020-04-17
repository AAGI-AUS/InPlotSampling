

JPSD0F <- function(pop, n, H, tau, N, K) {
  # tau: controls the ranking quality
  # n:sample size
  # H: Set szie
  # pop: population
  N <- length(pop) # population size
  # SRSI=sample(1:N,n,replace=TRUE)
  # SRS=pop[SRSI] # first create a simple randopm sample
  # redpop=pop[-SRSI] # remove the slected SRS from, the population
  # NR=length(redpop) # reduced population size
  pRIn <- 1:N #  reduced population index
  #################################################
  # below  consruct rank for each SRS unit post experimentally
  JPS <- matrix(0, ncol = (K + 1), nrow = n) # store JPS sample
  ##############################################
  for (i in (1:n)) {
    # Yi=SRS[i] # measured unit
    Compi <- sample(pRIn, H) # select H-1 unit to construct compariosn set
    Set <- pop[Compi] # combine H-1 unit with the  measured unit Y-i
    Yi <- Set[1]
    JPS[i, 1] <- Yi
    for (k in (2:(K + 1))) {
      DCSet <- Set + tau[k - 1] * rnorm(H, 0, 1) # adjust ranking quality using Dell-Clutter
      # model
      RankSet <- rank(DCSet) # rank the units
      JPS[i, k] <- RankSet[1] #  JPS sample for the i-th unit
    }
  }
  colnames(JPS) <- c("Y", paste("R", 1:K, sep = ""))
  return(JPS)
}

