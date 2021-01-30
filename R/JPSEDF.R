###########################################################
# This function Computes JPS estimator and its variance for each ranker  ##
###########################################################
# Y:  Response measurements
# RV: Ranks for Y
# set_size: Set Size
# N: Finite population size
# coef: Coefficients used in variance computation when sample size is n
# coef_del: Coefficients used in variance computation
#          when the i-th unit is deleted
# replace: replace =1 if the sampling is with replacement.
# model: model =1 if  super population model is used
#' Computes the estimator and variance for each individual ranker
#'
#' @param RV Rank values for Y
#' @param Y Response measurements
#' @param set_size
#' @param N
#' @param coef
#' @param coef_del
#' @param replace
#' @param model
#' @param K
#'
#' @return
#' @keywords internal
#'
JPSEDF <- function(RV, Y, set_size, N, coef, coef_del, replace, model, K) {
  n <- length(Y)
  M.est <- mean(aggregate(Y, data.frame(RV), mean)$x) # JPS estimate
  Y.ij <- expand.grid(Y, Y)

  GSV <- rep(0, set_size) # Group sample size vector
  TemSS <- aggregate(RV, data.frame(RV), length)
  GSV[TemSS[, 1]] <- TemSS$x # Judgment group sample sizes. Some would be zero.
  dn <- length(GSV[GSV > 0]) # the nonempty judgment groups
  dn.star <- length(GSV[GSV > 1]) # the number of judgment groups having at least two observations
  R.hhp <- expand.grid(RV, RV)
  Y.ij2 <- (Y.ij[, 1] - Y.ij[, 2])^2 #  squared differences of (Yi-Yj)^2
  G.sum <- aggregate(Y.ij2, R.hhp, sum) # group  squared differences based on judgment classes
  GS.size <- cbind(GSV[G.sum[, 1]], GSV[G.sum[, 2]]) # attach  judmgent group sample sizes
  # to each group
  denT2 <- GS.size[G.sum[, 1] == G.sum[, 2], 1] # determine nh from the judgment group set_size
  denT1 <- GS.size[G.sum[, 1] != G.sum[, 2], ] # determine sample sizes  n_h and n_h'for judgment groups set_size and set_size'
  den2 <- denT2 * (denT2 - 1) # determine the denumerator nh(nh-1) for T2
  numT2 <- G.sum[G.sum[, 1] == G.sum[, 2], 3] # sum_h sum_i sum_j ( Y_i=Y_j)^2I(R_i=set_size)I(R_j=set_size)
  TT2 <- sum(numT2[den2 > 0] / den2[den2 > 0]) # sum_h sum_i sum_j ( Y_i=Y_j)^2I(R_i=set_size)I(R_j=set_size')/(nh(nh-1))
  # Line below #sum_h sum_h'sum_i sum_j ( Y_i=Y_j)^2I(R_i=set_size)I(R_j=set_size')/(nh nh'))
  if (dim(denT1)[1] != 0) TT1 <- sum(G.sum[G.sum[, 1] != G.sum[, 2], 3] / (denT1[, 1] * (denT1[, 2]))) else TT1 <- 0
  ############################################################################
  # Variance estimate with full data
  M.Est <- mean(aggregate(Y, list(RV), mean)$x) # JPS estiamte with full data
  T2s <- set_size * TT2 / (2 * dn.star)
  T1s <- TT1 / (2 * coef[1] * dn^2)
  # VestD0=coef[2]*T1s/(set_size-1)+coef[3]*T2s
  if (!replace) {
    VEST <- coef[2] * T2s + coef[3] * (N - 1) * (T1s + T2s) / (N * (set_size - 1))
    if (VEST <= 0) VEST <- coef[2] * T2s / 2
  } else {
    VEST <- coef[2] * T1s / (set_size - 1) + coef[3] * T2s
  }
  if (model == 1) {
    VEST <- (T1s + T2s) / set_size^2 * ((-1 / N) + coef[2] * set_size^2 / (set_size - 1)) + T2s * ((coef[3] + coef[2]) - coef[2] * set_size / (set_size - 1))
    if (VEST <= 0) VEST <- T2s * ((coef[3] + coef[2]) - coef[2] * set_size / (set_size - 1))
  }
  if (K == 1) {
    ret <- c(M.Est, VEST)
    return(ret)
  }


  ################################################################


  ################################################################
  ######### This part is new for Jackknife replication, delete one observations
  ########  reduces the computation time
  ID <- 1:n # index to determine which observation is to be deleted
  Ind.ij <- expand.grid(ID, ID) # Index of observations to be deleted
  Y.ij2N <- Y.ij2[Ind.ij[, 1] - Ind.ij[, 2] != 0] # Remove Y_i=Y_ii
  R.hhpN <- R.hhp[Ind.ij[, 1] - Ind.ij[, 2] != 0, ] #  Remove R_i=R_i
  Ind.ij <- Ind.ij[Ind.ij[, 1] != Ind.ij[, 2], ] # Remove i=i
  #  deltM=matrix(0,ncol=6,nrow=n) # This stores the contribtuion of each
  # deleted observation to
  # T1 and T2

  INDM <- matrix(1:n, ncol = 1) # This is used in apply function  below
  PASS <- list(Y, RV, Ind.ij, GSV, Y.ij2N, R.hhpN, G.sum, TT2, TT1) # compile additional variables in list
  DeltM <- t(apply(INDM, 1, DELETi, PASS = PASS)) # This computes TT2, TT1, dn, dn-star
  # for each  deleted unit "i".
  # deltM=DeltM
  ##################################################
  T2v.Del <- set_size * DeltM[, 4] / (2 * DeltM[, 3]) # T2 when we delete the i-th unit
  # deltM[,4]: TT2i, deltM[,3]= dn_str, the number
  # of groups having at least two observations after deleting
  # the it-th unit
  T1v.Del <- DeltM[, 5] / (2 * coef_del[1] * (DeltM[, 2])^2) # T1 when we delete the i-th unit
  # deltM[,5]: TT1i, deltM[,2]= dn_str, the number
  # of groups having at least one observations after deleting
  # the it-th unit
  # T2s=set_size*sum(Y2hhT2*GSV1^2/(GSV1*(GSV1-1)))/(2*dn.star)
  # Y2hhT1=group.mean[group.mean[,1]- group.mean[,2]!=0,]$x
  # T1s=sum(Y2hhT1)/(2*coef[1]*dn^2)
  #  VestD0.del=coef_del[2]*T1v.Del/(set_size-1)+coef_del[3]*T2v.Del
  if (!replace) {
    VEST.Del <- coef_del[2] * T2v.Del + coef_del[3] * (N - 1) * (T1v.Del + T2v.Del) / (N * (set_size - 1))
    # if(VEST.Del <= 0) VEST.Del=coef_del[2]*T2v.del/2
    VEST.Del[VEST.Del <= 0] <- coef_del[2] * T2v.Del[VEST.Del <= 0] / 2
  } else {
    VEST.Del <- coef_del[2] * T1v.Del / (set_size - 1) + coef_del[3] * T2v.Del
  }
  if (model == 1) {
    VEST.Del <- (T1v.Del + T2v.Del) / set_size^2 * ((-1 / N) + coef_del[2] * set_size^2 / (set_size - 1)) + T2v.Del * ((coef_del[3] + coef_del[2]) - coef_del[2] * set_size / (set_size - 1))
    #    if(VEST.Del <= 0 ) VEST.Del=T2v.del*((coef_del[3]+coef_del[2])+coef_del[2]*set_size/(set_size-1))
    VEST.Del[VEST.Del <= 0] <- T2v.Del[VEST.Del <= 0] * ((coef_del[3] + coef_del[2]) - coef_del[2] * set_size / (set_size - 1))
  }

  Est <- c(M.Est, VEST) # M.Est: JPS estimate with sample size n
  # VEST:  varaince estimate of JPS estimator with sample size n
  Est.Del <- VEST.Del # n-dimentional vector containing
  # varince estimate of JPS estimator for each deleted observation
  ret <- c(Est, Est.Del, DeltM[, 6])
  # DeltM[,6] n-dimentional vector containing JPS estimate for
  # each deleted observation
  return(ret)
}
