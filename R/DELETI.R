DELETi <- function(i, PASS) {
  #  PASS=list(Y,RV,Ind.ij,GSV,Y.ij2N,R.hhpN,G.sum,TT2,TT1)
  Y <- PASS[[1]]
  RV <- PASS[[2]]
  Ind.ij <- PASS[[3]]
  GSV <- PASS[[4]]
  Y.ij2N <- PASS[[5]]
  R.hhpN <- PASS[[6]]
  G.sum <- PASS[[7]]
  TT2 <- PASS[[8]]
  TT1 <- PASS[[9]]
  n <- length(Y)
  # print(c(TT2,TT1))
  # print(deltM)
  ################################################################
  ######### This part is new for Jackknife replication, delete one observations
  ########  reduces the computation time
  # Ind.ij=expand.grid(ID,ID)  # Index of observations to be deleted
  # Y.ij2N=Y.ij2[Ind.ij[,1]-Ind.ij[,2]!=0]  # Remove Y_i=Y_ii
  # R.hhpN=R.hhp[Ind.ij[,1]-Ind.ij[,2]!=0,] #  Remove R_i=R_i
  # Ind.ij=Ind.ij[Ind.ij[,1]!=Ind.ij[,2],] # Remove i=i
  Ri <- RV[i] # Rank of the removed observation
  nh <- GSV[Ri] # Judgmnt group sample size of removed observaion
  RGS <- GSV
  RGS[Ri] <- RGS[Ri] - 1 # Judgmnet group sample size vector after i-th unit is removed
  JPSi <- mean(aggregate(Y[-i], list(RV[-i]), mean)$x) # JPS estimate after emoving the i-th unit
  dni <- length(RGS[RGS > 0]) # The number of non-empy judgment groups after removing the i-th unit
  dni.star <- length(RGS[RGS > 1]) # The number of judgment classes having atlest two observations
  # after we emoved the i-th unit
  icont1 <- Y.ij2N[Ind.ij[, 2] == i | Ind.ij[, 1] == i] # All squared differences (Y_r-Y_j)^2 that
  # contains Y_i in it
  rankVi.1 <- R.hhpN[Ind.ij[, 2] == i | Ind.ij[, 1] == i, ] # Ranks of R_r and R_j where  either r=i or j=i
  # or both
  T2iV <- icont1[rankVi.1[, 1] == rankVi.1[, 2]] # Extract squared differences where R_i=R_i
  if (nh == 1) {
    TT2i <- TT2
  } else {
    ContT2Ful <- sum(T2iV)
    # If nh=1 no contribution from the i-th unit since it is deleted
    TOT2 <- G.sum[as.logical((G.sum[, 1] == Ri) * (G.sum[, 2] == Ri)), ] # Find the contribution of
    # all squared differences in ranking group R_i
  }
  if (nh > 2) TT2i <- TT2 - TOT2[, 3] / (nh * (nh - 1)) + (TOT2[, 3] - ContT2Ful) / ((nh - 1) * (nh - 2))
  if (nh == 2) TT2i <- TT2 - TOT2[, 3] / (nh * (nh - 1))

  #####################
  #  T1 contribtuion
  if (nh == n) {
    TT1i <- 0
  } else {
    T1iV <- icont1[rankVi.1[, 1] != rankVi.1[, 2]] # Extract All (Y_i-Y_j)^2 that contains Y_i from h and h'
    GRind <- rankVi.1[rankVi.1[, 1] != rankVi.1[, 2], ] # Exctract the ranks R_i and R_j of Y_i and Y_j
    GRind <- GRind[, 1] * GRind[, 2] # Multiply the ranks of R_i and R_j
    T1iV <- aggregate(T1iV, list(GRind), sum) # Add all squared differences in the same ranking groups
    ContT1Ful <- sum(T1iV / (nh * GSV[T1iV[, 1] / Ri])) # The contribution of the i-th unit to T1
    LOGIC <- as.logical((G.sum[, 1] == Ri) * (G.sum[, 2] != Ri) | as.logical((G.sum[, 1] != Ri) * (G.sum[, 2] == Ri)))
    TOT1 <- G.sum[LOGIC, ] # Extract contribution of the i-th unit in ranking groups R_i=h and R_j=h'
    TGRind <- TOT1[, 1] * TOT1[, 2]
    TOT1 <- aggregate(TOT1[, 3], list(TGRind), sum) # sum over all R_j=h'
    sumT1 <- sum(TOT1[, 2] / (nh * GSV[TOT1[, 1] / Ri])) # Find he contribtuion all squared differences
    # containing Y_i  over all ranking grpoups
    if (nh > 1) TT1i <- TT1 - sumT1 + sum(((TOT1[, 2]) - (T1iV[, 2])) / ((nh - 1) * GSV[TOT1[, 1] / Ri])) else TT1i <- TT1 - sumT1
  }
  return(c(i, dni, dni.star, TT2i, TT1i, JPSi))
  # i: id of the deleted observation
  # dni: d_n after the i-th unit is deleted
  # dni.star: d_n^star after the i-th unit is deleted
  # TT2i: T_2 after the i-th unit is deleted
  # TT1i: T_1 after the i-th unit is deleted
  # JPSi: JPS estiamte after the i-th unit is delelted
}
