ListF <- function(Data, Setsize, Replace, N, Model, CoefD0) {
    Y <- Data[, 1]
    Ranks <- Data[, -1]
    n <- length(Y)
    H <- Setsize
    # CoefD0=CoefD0F(Setsize,n)
    # print(c(N,Replace))
    ##################################################################
    #  For design D2 coefficients
    if (Replace == 1) {
        if (is.null(N)) {
            print("Population size N must be provided for without replacement sampling")
        }
        coef1D2 <- CoefD0[1]
        coef2D2 <- 1 / (H * (H - 1)) + CoefD0[3] + CoefD0[2] - (CoefD0[2] + 1 / H^2) * H / (H - 1)
        coef3D2 <- CoefD0[2] - 1 / (N - 1) * (1 / H - (CoefD0[1] + 1 / H^2))
        CoefD <- c(coef1D2, coef2D2, coef3D2)
    } else {
        CoefD <- CoefD0
    }
    if (Model == 1) CoefD <- CoefD0
    ##################################################################
    # print(Ranks)
    WEIGHT <- t(apply(data.frame(Ranks), 1, WEIGHTF, H = Setsize))
    # print(WEIGHT)
    WY <- Y %*% WEIGHT
    Eff.Ssize <- apply(WEIGHT, 2, sum)
    W.est <- mean(WY[Eff.Ssize > 0] / Eff.Ssize[Eff.Ssize > 0])

    EST <- t(apply(data.frame(Ranks), 2, JPSED0F, Y = Y, H = Setsize, Coef = CoefD, N, Replace, Model)) # Estimate of population mean and variance
    # for each ranking method
    # print("From ListF" )
    #  print(EST)
    # print(EST)
    prec.weight <- (1 / EST[, 2]) / sum(1 / EST[, 2]) # precison weight-- inverse of the variance
    Cest <- sum(prec.weight * EST[, 1]) # point estimate for the population mean
    # JackNrep=matrix(0,nrow=n,ncol=2)
    #  CoefD0n.1=CoefD0F(Setsize,(n-1))
    retest <- c(mean(EST[, 1]), Cest, W.est)
    return(retest)
}
