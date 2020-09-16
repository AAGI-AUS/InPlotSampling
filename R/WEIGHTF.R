#' Title
#'
#' @param RV
#' @param H
#'
#' @return
#' @export
#'
#' @examples
WEIGHTF <- function(RV, H) {
    UniquR <- unique(RV) #Get unique values in row
    weightV <- rep(0, H) #Preallocate vector for weights?
    K <- length(RV) #Number of rankers
    for (h in 1:length(UniquR)) { #For each of the unique ranks
        UR <- UniquR[h] #Get each of the unique ranks
        weightV[UR] <- length(RV[RV == UR]) / K #Calculate what proportion of the rankers selected each rank
    }
    return(weightV)
}
