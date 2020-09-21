############################## 3
# This function genrates RSS sample with K ranking methods with replacemet
# Pop: has two variables popY, variable of interest
#  popAux: Auxiliary variable
# We assume pop and popAux are correlated
# n: sample size n=Hd, H: set size, d: cycle size
RSSDF <- function(pop, n, H, K) {
  d <- n / H
  popY <- pop[, 1]
  popAux <- pop[, 2]
  N <- length(popY)
  RSSM <- matrix(0, ncol = (K + 1), nrow = n)
  ic <- 1
  for (j in (1:d)) {
    for (h in (1:H)) {
      setid <- sample(1:N, H)
      setY <- popY[setid]
      setX <- popAux[setid]
      orAux <- order(setX)
      orY <- setY[orAux]
      osetX <- setX[orAux]
      setidO <- setid[orAux]
      # oset=DELLF(set,tauV[1])
      RSSM[ic, c(1, 2)] <- c(orY[h], h)
      k1obs <- osetX[h]
      redAux <- popAux[-setidO[h]]
      if (K > 1) {
        for (k in (2:K)) {
          kset <- c(k1obs, sample(redAux, (H - 1)))
          OKS <- order(kset)
          koset <- kset[OKS]
          kR <- which(koset == k1obs)
          if (length(kR) > 1) kR <- sample(kR, 1)
          RSSM[ic, (k + 1)] <- kR
        }
      }
      ic <- ic + 1
    }
  }
  return(RSSM)
}
