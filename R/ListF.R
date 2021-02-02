#' Title
#'
#' @param data The data
#' @param set_size set size
#' @param Replace replacement
#' @param N Population size
#' @param Model Model
#' @param CoefD0 Coef
#'
#' @return
#' @keywords internal
#'
#' @examples
ListF <- function(data, set_size, Replace, N, Model, CoefD0) {
    Y <- data[, 1]
    Ranks <- data[, -1]
    n <- length(Y)
    # set_size <- set_size
    # CoefD0=CoefD0F(set_size,n)
    # print(c(N,Replace))
    ##################################################################
    #  For design D2 coefficients
    if (Replace == 1) {
        if (is.null(N)) {
            print("Population size N must be provided for without replacement sampling")
        }
        coef1D2 <- CoefD0[1]
        coef2D2 <- 1 / (set_size * (set_size - 1)) + CoefD0[3] + CoefD0[2] - (CoefD0[2] + 1 / set_size^2) * set_size / (set_size - 1)
        coef3D2 <- CoefD0[2] - 1 / (N - 1) * (1 / set_size - (CoefD0[1] + 1 / set_size^2))
        CoefD <- c(coef1D2, coef2D2, coef3D2)
    } else {
        CoefD <- CoefD0
    }
    if (Model == 1) CoefD <- CoefD0
    ##################################################################
    # print(Ranks)
    WEIGHT <- t(apply(data.frame(Ranks), 1, WEIGHTF, H = set_size))
    # print(WEIGHT)
    WY <- Y %*% WEIGHT
    Eff.Ssize <- colSums(WEIGHT)
    W.est <- mean(WY[Eff.Ssize > 0] / Eff.Ssize[Eff.Ssize > 0])

    # This line is the culprit for time
    EST <- t(apply(data.frame(Ranks), 2, JPSED0F, Y = Y, set_size = set_size, Coef = CoefD, N, Replace, Model)) # Estimate of population mean and variance
    # for each ranking method
    # print("From ListF" )
    #  print(EST)
    # print(EST)
    prec.weight <- (1 / EST[, 2]) / sum(1 / EST[, 2]) # precison weight-- inverse of the variance
    Cest <- sum(prec.weight * EST[, 1]) # point estimate for the population mean
    # JackNrep=matrix(0,nrow=n,ncol=2)
    #  CoefD0n.1=CoefD0F(set_size,(n-1))
    retest <- c(mean(EST[, 1]), Cest, W.est)
    return(retest)
}
