#' Calculate Agreement Weights
#'
#' @param RV Ranking values
#' @param set_size Set size
#'
#' @return A vector of agreement weights
#' @keywords internal
#'
WEIGHTF <- function(RV, set_size) {
  UniquR <- unique(RV)
  weightV <- rep(0, set_size)
  K <- length(RV)
  for (h in (1:length(UniquR))) {
    UR <- UniquR[h]
    weightV[UR] <- length(RV[RV == UR]) / K
  }
  return(weightV)
}
