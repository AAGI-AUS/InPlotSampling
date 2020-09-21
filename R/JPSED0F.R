#' Title
#'
#' @param RV Ranking values provided
#' @param Y The data values that were ranked
#' @param set_size The set
#' @param Coef
#' @param N
#' @param Replace
#' @param Model
#'
#' @importFrom dplyr group_by summarise
#'
#' @return A vector with two elements: the mean and variance estimate from JPS
#'
#' @keywords internal
#'
JPSED0F <- function(RV, Y, set_size, Coef, N, Replace, Model) {

  ###########################################################
  # This function Computes JPS estimator and its variance  ##
  ###########################################################
  # JPSD0:
  # First column: Response
  # Second column: Ranks
  # print(Coef)
  RVD <- data.frame(RV, Y)
  M.est <- mean(dplyr::summarise(dplyr::group_by(RVD, RV), x = mean(Y))$x) # JPS estimate
  YIYJ <- expand.grid(Y, Y)
  GSample.Size <- dplyr::summarise(dplyr::group_by(RVD, RV), x = n())$x
  # dn <- length(GSample.Size)
  # print(dn)
  GSample.Size1 <- GSample.Size[GSample.Size > 1]
  # dn.star <- length(GSample.Size1)
  RhRhp <- expand.grid(RV, RV)
  YIYJ2 <- (YIYJ[, 1] - YIYJ[, 2])^2
  # group.mean <- aggregate(YIYJ2, RhRhp, mean)
  data <- cbind(RhRhp, YIYJ2)
  group.mean <- dplyr::summarise(dplyr::group_by(data, Var1, Var2), x = mean(YIYJ2))
  Y2hhT2 <- group.mean[group.mean[, 1] - group.mean[, 2] == 0, ]$x
  Y2hhT2 <- Y2hhT2[GSample.Size > 1]
  T2s <- set_size * sum(Y2hhT2 * GSample.Size1^2 / (GSample.Size1 * (GSample.Size1 - 1))) / (2 * length(GSample.Size1))
  Y2hhT1 <- group.mean[group.mean[, 1] - group.mean[, 2] != 0, ]$x
  T1s <- sum(Y2hhT1) / (2 * Coef[1] * length(GSample.Size)^2)
  VestD0 <- Coef[2] * T1s / (set_size - 1) + Coef[3] * T2s
  if (Replace == 1) {
    VEST <- Coef[2] * T2s + Coef[3] * (N - 1) * (T1s + T2s) / (N * (set_size - 1))
    if (VEST <= 0) VEST <- Coef[2] * T2s / 2
  } else {
    VEST <- Coef[2] * T1s / (set_size - 1) + Coef[3] * T2s
  }
  if (Model == 1) {
    VEST <- (T1s + T2s) / set_size^2 * ((-1 / N) + Coef[2] * set_size / (set_size - 1)) + T2s * ((Coef[3] + Coef[2]) + Coef[2] * set_size / (set_size - 1))
    if (VEST <= 0) VEST <- T2s * ((Coef[3] + Coef[2]) + Coef[2] * set_size / (set_size - 1))
  }
  return(c(M.est, VEST))
}

JPSED0F_orig <- function(RV, Y, set_size, Coef, N, Replace, Model) {
  ###########################################################
  # This function Computes JPS estimator and its variance  ##
  ###########################################################
  # JPSD0:
  # First column: Response
  # Second column: Ranks
  # print(Coef)
  RVD <- data.frame(RV)
  M.est <- mean(aggregate(Y, RVD, mean)$x) # JPS estimate
  YIYJ <- expand.grid(Y, Y)
  GSample.Size <- aggregate(RV, data.frame(RV), length)$x
  dn <- length(GSample.Size)
  # print(dn)
  GSample.Size1 <- GSample.Size[GSample.Size > 1]
  dn.star <- length(GSample.Size1)
  RhRhp <- expand.grid(RV, RV)
  YIYJ2 <- (YIYJ[, 1] - YIYJ[, 2])^2
  group.mean <- aggregate(YIYJ2, RhRhp, mean)
  Y2hhT2 <- group.mean[group.mean[, 1] - group.mean[, 2] == 0, ]$x
  Y2hhT2 <- Y2hhT2[GSample.Size > 1]
  T2s <- set_size * sum(Y2hhT2 * GSample.Size1^2 / (GSample.Size1 * (GSample.Size1 - 1))) / (2 * dn.star)
  Y2hhT1 <- group.mean[group.mean[, 1] - group.mean[, 2] != 0, ]$x
  T1s <- sum(Y2hhT1) / (2 * Coef[1] * dn^2)
  VestD0 <- Coef[2] * T1s / (set_size - 1) + Coef[3] * T2s
  if (Replace == 1) {
    VEST <- Coef[2] * T2s + Coef[3] * (N - 1) * (T1s + T2s) / (N * (set_size - 1))
    if (VEST <= 0) VEST <- Coef[2] * T2s / 2
  } else {
    VEST <- Coef[2] * T1s / (set_size - 1) + Coef[3] * T2s
  }
  if (Model == 1) {
    VEST <- (T1s + T2s) / set_size^2 * ((-1 / N) + Coef[2] * set_size / (set_size - 1)) + T2s * ((Coef[3] + Coef[2]) + Coef[2] * set_size / (set_size - 1))
    if (VEST <= 0) VEST <- T2s * ((Coef[3] + Coef[2]) + Coef[2] * set_size / (set_size - 1))
  }
  return(c(M.est, VEST))
}

JPSD0F_new <- function(pop, n, H, tau, N, K) {
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
    Compi <- sample(pRIn, H) # select H-1 unit to construct comparison set
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
