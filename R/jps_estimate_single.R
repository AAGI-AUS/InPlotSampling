#' Computes the estimator and variance for each individual ranker
#'
#' @param ranks Ranks of Y.
#' @param y Response measurements.
#' @param set_size Set size for each ranking group.
#' @param N Finite population size.
#' @param coef Coefficients used in variance computation when sample size is n.
#' @param coef_del Coefficients used in variance computation when the i-th unit is deleted.
#' @param replace Logical. Sample with replacement?
#' @param model_based An inference mode:
#' - `FALSE`: design based inference
#' - `TRUE`: model based inference using super population model
#' @param K Number of rankers.
#'
#' @return A `data.frame` with the point estimates provided by JPS estimators along with standard error and
#'   confidence intervals.
#' @keywords internal
#'
jps_estimate_single <- function(ranks, y, set_size, N, coef, coef_del, replace, model_based, K) {
  y_ij <- expand.grid(y, y)

  # count ranks including zeros
  rank_count <- rep(0, set_size)
  agg_rank <- aggregate(ranks, data.frame(ranks), length)
  rank_count[agg_rank$ranks] <- agg_rank$x

  n_non_empty_ranks <- dim(agg_rank)[[1]]
  n_two_plus_ranks <- length(rank_count[rank_count > 1])

  rank_ij <- expand.grid(ranks, ranks)
  y_ij_diff2 <- (y_ij[, 1] - y_ij[, 2])^2

  # group squared differences based on judgement classes
  agg_ss <- aggregate(y_ij_diff2, rank_ij, sum)
  paired_count <- cbind(rank_count[agg_ss[, 1]], rank_count[agg_ss[, 2]])
  count_ii <- paired_count[agg_ss[, 1] == agg_ss[, 2], 1]
  count_non_ii <- paired_count[agg_ss[, 1] != agg_ss[, 2], ]

  # nh(nh - 1)
  nh_nh1 <- count_ii * (count_ii - 1)
  agg_ss_ii <- agg_ss[agg_ss[, 1] == agg_ss[, 2], 3]
  # sum_h sum_i sum_j (Y_i=Y_j)^2I(R_i=set_size)I(R_j=set_size')/(nh(nh-1))
  tt2 <- sum(agg_ss_ii[nh_nh1 > 0] / nh_nh1[nh_nh1 > 0])
  if (dim(count_non_ii)[1] != 0) {
    # sum_h sum_h'sum_i sum_j ( Y_i=Y_j)^2I(R_i=set_size)I(R_j=set_size')/(nh nh'))
    tt1 <- sum(agg_ss[agg_ss[, 1] != agg_ss[, 2], 3] / (count_non_ii[, 1] * (count_non_ii[, 2])))
  } else {
    tt1 <- 0
  }

  ############################################################################
  estimated_mean <- mean(aggregate(y, list(ranks), mean)$x)
  t2s <- set_size * tt2 / (2 * n_two_plus_ranks)
  t1s <- tt1 / (2 * coef[1] * n_non_empty_ranks^2)
  # VestD0=coef[2]*T1s/(set_size-1)+coef[3]*T2s
  if (!replace) {
    estimated_variance <- coef[2] * t2s + coef[3] * (N - 1) * (t1s + t2s) / (N * (set_size - 1))
    if (estimated_variance <= 0) {
      estimated_variance <- coef[2] * t2s / 2
    }
  } else {
    estimated_variance <- coef[2] * t1s / (set_size - 1) + coef[3] * t2s
  }

  if (model_based) {
    estimated_variance <- ((t1s + t2s) / set_size^2 * ((-1 / N) + coef[2] * set_size^2 / (set_size - 1))
      + t2s * ((coef[3] + coef[2]) - coef[2] * set_size / (set_size - 1)))

    if (estimated_variance <= 0) {
      estimated_variance <- t2s * ((coef[3] + coef[2]) - coef[2] * set_size / (set_size - 1))
    }
  }
  if (K == 1) {
    return(c(estimated_mean, estimated_variance))
  }

  ################################################################
  ######### This part is new for Jackknife replication, delete one observations
  ########  reduces the computation time
  n <- length(y)
  # index to determine which observation is to be deleted
  ID <- 1:n
  # Index of observations to be deleted
  Ind.ij <- expand.grid(ID, ID)
  # Remove Y_i=Y_ii
  Y.ij2N <- y_ij_diff2[Ind.ij[, 1] - Ind.ij[, 2] != 0]
  #  Remove R_i=R_i
  R.hhpN <- rank_ij[Ind.ij[, 1] - Ind.ij[, 2] != 0, ]
  # Remove i=i
  Ind.ij <- Ind.ij[Ind.ij[, 1] != Ind.ij[, 2], ]
  #  deltM=matrix(0,ncol=6,nrow=n) # This stores the contribtuion of each
  # deleted observation to
  # T1 and T2

  # This is used in apply function  below
  INDM <- matrix(1:n, ncol = 1)
  # compile additional variables in list
  PASS <- list(y, ranks, Ind.ij, rank_count, Y.ij2N, R.hhpN, agg_ss, tt2, tt1)
  # This computes TT2, TT1, dn, dn-star for each  deleted unit "i".deltM=DeltM
  DeltM <- t(apply(INDM, 1, DELETi, PASS = PASS))
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
  if (model_based) {
    VEST.Del <- (T1v.Del + T2v.Del) / set_size^2 * ((-1 / N) + coef_del[2] * set_size^2 / (set_size - 1)) + T2v.Del * ((coef_del[3] + coef_del[2]) - coef_del[2] * set_size / (set_size - 1))
    #    if(VEST.Del <= 0 ) VEST.Del=T2v.del*((coef_del[3]+coef_del[2])+coef_del[2]*set_size/(set_size-1))
    VEST.Del[VEST.Del <= 0] <- T2v.Del[VEST.Del <= 0] * ((coef_del[3] + coef_del[2]) - coef_del[2] * set_size / (set_size - 1))
  }

  Est <- c(estimated_mean, estimated_variance) # M.Est: JPS estimate with sample size n
  # VEST:  varaince estimate of JPS estimator with sample size n
  Est.Del <- VEST.Del # n-dimentional vector containing
  # varince estimate of JPS estimator for each deleted observation
  ret <- c(Est, Est.Del, DeltM[, 6])
  # DeltM[,6] n-dimentional vector containing JPS estimate for
  # each deleted observation
  return(ret)
}
