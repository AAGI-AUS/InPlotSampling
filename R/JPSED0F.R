#' Title
#'
#' @param RV
#' @param Y
#' @param H
#' @param Coef
#' @param N
#' @param Replace
#' @param Model
#'
#' @return
#' @export
#'
#' @examples
JPSED0F <- function(RV, Y, H, Coef, N, Replace, Model) {
    ###########################################################
    # This function Computes JPS estimator and its variance  ##
    ###########################################################
    # JPSD0:
    # First column: Response
    # Second column: Ranks
    # print(Coef)
    RVD <- data.frame(RV)
    M.est <- mean(aggregate(Y, RVD, mean)$x) # JPS estimate
    YIYJ <- expand.grid(Y, Y)
    GSample.Size <- aggregate(RV, data.frame(RV), length)$x
    dn <- length(GSample.Size)
    # print(dn)
    GSample.Size1 <- GSample.Size[GSample.Size > 1]
    dn.star <- length(GSample.Size1)
    RhRhp <- expand.grid(RV, RV)
    YIYJ2 <- (YIYJ[, 1] - YIYJ[, 2])^2
    group.mean <- aggregate(YIYJ2, RhRhp, mean)
    Y2hhT2 <- group.mean[group.mean[, 1] - group.mean[, 2] == 0, ]$x
    Y2hhT2 <- Y2hhT2[GSample.Size > 1]
    T2s <- H * sum(Y2hhT2 * GSample.Size1^2 / (GSample.Size1 * (GSample.Size1 - 1))) / (2 * dn.star)
    Y2hhT1 <- group.mean[group.mean[, 1] - group.mean[, 2] != 0, ]$x
    T1s <- sum(Y2hhT1) / (2 * Coef[1] * dn^2)
    VestD0 <- Coef[2] * T1s / (H - 1) + Coef[3] * T2s
    if (Replace == 1) {
        VEST <- Coef[2] * T2s + Coef[3] * (N - 1) * (T1s + T2s) / (N * (H - 1))
        if (VEST <= 0) VEST <- Coef[2] * T2s / 2
    } else {
        VEST <- Coef[2] * T1s / (H - 1) + Coef[3] * T2s
    }
    if (Model == 1) {
        VEST <- (T1s + T2s) / H^2 * ((-1 / N) + Coef[2] * H / (H - 1)) + T2s * ((Coef[3] + Coef[2]) + Coef[2] * H / (H - 1))
        if (VEST <= 0) VEST <- T2s * ((Coef[3] + Coef[2]) + Coef[2] * H / (H - 1))
    }
    return(c(M.est, VEST))
}
