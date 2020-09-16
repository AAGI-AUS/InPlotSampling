#' Title
#'
#' @param H
#' @param n
#'
#' @return
#' @export
#'
#' @examples
CoefF <- function(H, n) {
    #####################################################################
    #####################################################################
    #  THis function computes the coefficient of variance estimator  ####
    #####################################################################
    # H: set size
    # n: sample size
    kv <- 1:H
    ######################################################
    # Expected value of I_1^2/d_n^2    ###################
    E.I2.dn2 <- sum((kv / H)^(n - 1)) / H^2 # E{(I_1/d_n)^2}    ##
    ######################################################

    ######################################################
    # Compute the expected value I_1^2/(n_1 d_n^2)      ##
    ######################################################

    indM <- rep(0, 3)
    for (k in (2:H)) {
        AA <- expand.grid.alt((1:(k - 1)), 1:(n - k + 1))
        TEm <- cbind(rep(k, dim(AA)[1]), AA)
        indM <- rbind(indM, TEm)
    }
    indM <- indM[-1, ]
    indM <- as.matrix(indM)
    triplesum <- apply(indM, 1, TRIPLEF, n = n, H = H)
    E.I2.n1dn2 <- (1 / n + sum(triplesum)) / H^n

    # E.I2.n1dn2 <- (1/H^n)*(1/n+tot)
    ######################################################
    ######################################################
    VarI1.dn <- E.I2.dn2 - (1 / H^2)
    coef1D0 <- 1 / (H^2) - VarI1.dn / (H - 1)
    coef2D0 <- VarI1.dn
    coef3D0 <- E.I2.n1dn2 - VarI1.dn

    CoefD0 <- c(coef1D0, coef2D0, coef3D0)
    return(CoefD0)
}
