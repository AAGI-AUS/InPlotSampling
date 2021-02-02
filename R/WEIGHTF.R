#' Title
#'
#' @param RV
#' @param set_size Set size
#'
#' @return
#' @keywords internal
#'
#' @examples
WEIGHTF <- function(RV, set_size) {
  UniquR <- unique(RV)
  weightV <- rep(0, H)
  K <- length(RV)
  for (h in (1:length(UniquR))) {
    UR <- UniquR[h]
    weightV[UR] <- length(RV[RV == UR]) / K
  }
  return(weightV)
}
