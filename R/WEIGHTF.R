WEIGHTF <- function(RV, H) {
  UniquR <- unique(RV)
  weightV <- rep(0, H)
  K <- length(RV)
  for (h in (1:length(UniquR))) {
    UR <- UniquR[h]
    weightV[UR] <- length(RV[RV == UR]) / K
  }
  return(weightV)
}
