#' Title
#'
#' @param pop A vector of values which is the population samples are taken from.
#' @param sample_size The size of the sample to be taken.
#' @param set_size
#' @param number_rankers The number of rankers.
#' @param tau The ranking quality.
#'
#' @return
#' @export
#'
#' @examples
JPSD0F <- function(pop, sample_size, set_size, number_rankers, tau) {

  # This file is
  # tau: controls the ranking quality
  # n:sample size
  # set_size/H: Set size
  # pop: population


  # Check set_size <= sample_size?
  # Can we sample with replacement?
  # Population should be vector of results. Alternatively pass dataframe and name of column?
  # How is tau decided/selected? Should it have a default value? What values can it take, and what do they mean?

  pop_size <- length(pop) # population size

  if(pop_size < sample_size) {
    stop("sample_size is larger than the population.")
  }

  # SRSI=sample(1:N,n,replace=TRUE)
  # SRS=pop[SRSI] # first create a simple random sample
  # redpop=pop[-SRSI] # remove the selected SRS from, the population
  # NR=length(redpop) # reduced population size
  pRIn <- 1:pop_size #  reduced population index
  #################################################
  # below  construct rank for each SRS unit post experimentally
  JPS <- matrix(0, ncol = (number_rankers + 1), nrow = sample_size) # store JPS sample
  ##############################################
  for (i in (1:sample_size)) {
    # Yi=SRS[i] # measured unit
    Compi <- sample(pRIn, set_size) # select H-1 unit to construct comparison set
    Set <- pop[Compi] # combine H-1 unit with the  measured unit Y-i
    Yi <- Set[1]
    JPS[i, 1] <- Yi
    for (k in (2:(number_rankers + 1))) {
      DCSet <- Set + tau[k - 1] * rnorm(set_size, 0, 1) # adjust ranking quality using Dell-Clutter
      # model
      RankSet <- rank(DCSet) # rank the units
      JPS[i, k] <- RankSet[1] #  JPS sample for the i-th unit
    }
  }
  colnames(JPS) <- c("Y", paste("R", 1:number_rankers, sep = ""))
  return(JPS)
}

