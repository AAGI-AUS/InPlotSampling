#' Title
#'
#' @param pop
#' @param n
#' @param H
#' @param tau
#' @param N
#' @param K
#'
#' @return
#' @export
#'
#' @examples
JPSD2F <- function(pop, n, H, tau, N, K) {
    #############################################
    # Ths function generates JPS sample        ##
    #############################################
    # K: Number of rankers
    # tau: controls the ranking quality
    # n:sample size
    # H: Set szie
    # pop: population
    N <- length(pop) # population size
    nsets <- matrix(sample(pop, n * H), ncol = H, nrow = n)
    #################################################
    # below  consruct rank for each SRS unit post experimentally
    JPS <- matrix(0, ncol = K + 1, nrow = n) # store JPS sample
    ##############################################
    for (i in (1:n)) {
        Set <- nsets[i, ] # select compariosn set i
        tem <- rep(0, K) # initialize to store ranks of he rankers for ocm,parion set i
        for (k in (1:K)) {
            DCSet <- Set + tau[k] * rnorm(H, 0, 1) # adjust ranking quality using Dell-Clutter
            # model
            RankSet <- rank(DCSet) # ranks the units in the comparion set i by ranker k
            tem[k] <- RankSet[1] #  the rank of the i-th mesured unit by ranker k
        }
        JPS[i, ] <- c(Set[1], tem) # meaured value of unit i and ranks by k rankers
    }
    colnames(JPS) <- c("Y", paste("R", 1:K, sep = ""))
    return(JPS)
}
