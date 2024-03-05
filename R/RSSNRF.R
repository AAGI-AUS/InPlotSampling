############################## 3
# This function genrates RSS sample with K ranking methods without replacemet
# Pop: has two variables popY, variable of interest
#  popAux: Auxiliary variable
# We assume pop and popAux are correlated
# n: sample size n=Hd, H: set size, d: cycle size
RSSNRF <- function(pop, n, H, K) {
  verify_rss_params(pop, n, H, K)

  n_cycles <- n / H
  K1 <- K + 1
  rseq <- rep((1:H), times = n_cycles)
  popY <- pop[, 1]
  N <- length(popY)
  popAux <- pop[, 2]
  popind <- 1:N
  RSSM <- matrix(0, ncol = (K1), nrow = n)
  # unused variable
  # ic <- 1
  ind <- sample(popind, n * H)
  setY <- matrix(popY[ind], ncol = H, nrow = n)
  setX <- matrix(popAux[ind], ncol = H, nrow = n)
  indexM <- matrix(ind, ncol = H, nrow = n)
  setYX <- cbind(rseq, setY, setX, indexM)
  ordYX <- t(apply(setYX, 1, ORD))
  RSSM[, c(1, 2)] <- c(ordYX[, 1], rseq)
  # redpop=popAux[-ordYX[,3]]
  Yind <- ordYX[, 3]
  redpopind <- popind[-Yind]
  Xh <- ordYX[, 2]
  # Nk=length(redpop)
  if (K1 > 2) {
    for (k in (3:K1)) {
      indk <- sample(redpopind, n * (H - 1))
      setX <- matrix(popAux[indk], ncol = (H - 1), nrow = n)
      indexM <- matrix(indk, ncol = (H - 1), nrow = n)
      indexM <- cbind(Yind, indexM)
      setX <- cbind(Xh, setX)
      setYX <- cbind(Xh, setX, indexM)
      RankK <- apply(setYX, 1, ORDk)
      RSSM[, k] <- RankK
    }
  }

  return(RSSM)
}
