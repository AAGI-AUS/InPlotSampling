###########################################
#  This function provides estimator for RSS data
#  RSSK: n by (K+1) dimensional data matrix, the first column is Y-values,
#  the next K columns are the ranks of K-ranking methods
#  set_size: set Size
# N: population size
# model: if Modle=0 design based inference, if model=1, superpopulation model
# replace: if replace=TRUE with replacmenet selection is used.
# If replace=FALSE without replacement selection is used
# B: Bootstrap replication
#' This function provides estimator for RSS data
#'
#' @param data An n by (K+1) dimensional data matrix, the first column is Y-values, the next K coulumns are the ranks of K-ranking methods
#' @param set_size The set size
#' @param replace Logical. Sample with replacement?
#' @param model If Modle=0, use design based inference, if model=1, use superpopulation model
#' @param N The population size
#' @param alpha The significance level
#'
#' @return
#' @keywords internal
#'
rss_estimate <- function(data, set_size, replace, model, N, alpha) {
  # unused variable
  # RM <- data[, -1]
  RV <- data[, 2] # We need to be careful about this.
  Y <- data[, 1] # We need to be careful about this. Need to ensure response is in col 1.
  n <- nrow(data)
  K <- ncol(data) - 1
  #########################################################################
  ########## Single ranker RSS estimator without replacement design
  if (!replace) {
    RSS.oneE <- mean(aggregate(Y, list(RV), FUN = mean)$x) # Single ranker RSS estimate
    EST <- RSSED2F(RV, Y, set_size, N)
    ### Confidence interval balanced RSS
    LC <- EST[1] - qt(1 - alpha / 2, n - 1) * sqrt(EST[2])
    UC <- EST[1] + qt(1 - alpha / 2, n - 1) * sqrt(EST[2])
  }
  #############################################################################
  ###############################################################################

  ####################################################################################
  ########### Single ranker RSS estimator with replacement design
  if (replace) {
    RSS.oneE <- mean(aggregate(Y, list(RV), FUN = mean)$x) # Single ranker RSS estimate
    RSS.oneCount <- aggregate(RV, list(RV), FUN = length)$x
    RSS.oneVar <- aggregate(Y, list(RV), FUN = var)$x
    Hd <- length(RSS.oneVar[RSS.oneCount > 1])
    RSS.oneVE <- sum(RSS.oneVar[RSS.oneCount > 1] / RSS.oneCount[RSS.oneCount > 1]) / Hd^2
    EST <- c(RSS.oneE, RSS.oneVE)
    LC <- EST[1] - qt(1 - alpha / 2, n - 1) * sqrt(EST[2])
    UC <- EST[1] + qt(1 - alpha / 2, n - 1) * sqrt(EST[2])
  }
  ######################################################################################
  ########################################################################################

  ##########################################################################################
  ######## Single ranker RSS estimator with under super population model ##################
  if (model == 1) {
    RSS.oneE <- mean(aggregate(Y, list(RV), FUN = mean)$x) # Single ranker RSS estimate
    EST <- RSSED2F(RV, Y, set_size, N)
    LC <- EST[1] - qt(1 - alpha / 2, n - 1) * sqrt(EST[2])
    UC <- EST[1] + qt(1 - alpha / 2, n - 1) * sqrt(EST[2])
  }

  Lower.limit <- round(c(LC), digits = 3)
  Upper.limit <- round(c(UC), digits = 3)
  ST.error <- sqrt(EST[2])
  Point.est <- EST[1]
  if (K == 1) {
    Confinterval <- paste(Lower.limit, Upper.limit, sep = ",")
    Estimator <- c("RSS-1")
    summary.table <- data.frame(Estimator, round(Point.est, digits = 3), round(ST.error, digits = 3), Confinterval, stringsAsFactors = F)
    colnames(summary.table) <- c("Estimator", "point.est", "St.error", paste((1 - alpha) * 100, "% Confidence Interval", sep = ""))
    return(summary.table)
  }


  #################################### 33
  # agreement weight estimator
  AW <- data[, -1] # Ranks
  AW <- t(apply(data.frame(data[, -1]), 1, calculate_agreement_weights, set_size)) # agreemeent weights
  eff.SS <- apply(AW, 2, sum)
  Crosprod <- data[, 1] %*% AW
  W.est <- mean(Crosprod[eff.SS > 0] / eff.SS[eff.SS > 0]) # RSS agreement weight estiamtor
  AWY <- cbind(data[, 1], AW)
  Jack.Repl.AWi <- apply(matrix(1:n, ncol = 1), 1, FWDel1, AWY = AWY) # Aggrement weight estimator
  # when the i-th obseervation is deleted
  if (replace) fc <- 1 else fc <- 1 - n / (N - 1)
  J.var <- fc * (n - 1) * var(Jack.Repl.AWi) * ((n - 1) / n)^2 # Jackknife variance estimate  for aggreement weight JPS estimator
  ##############################################################



  # Jackknife confidence interval based on weighted estimator
  LWC <- W.est - qt(1 - alpha / 2, n - 1) * sqrt(J.var)
  UWC <- W.est + qt(1 - alpha / 2, n - 1) * sqrt(J.var)


  ############################################################################################
  #############################################################################################
  Lower.limit <- round(c(LC, LWC), digits = 3)
  Upper.limit <- round(c(UC, UWC), digits = 3)
  ST.error <- c(sqrt(EST[2]), sqrt(J.var))
  Point.est <- c(EST[1], W.est)
  Confinterval <- paste(Lower.limit, Upper.limit, sep = ",")
  Estimator <- c("RSS-1", " Aggregate Weighted")
  summary.table <- data.frame(Estimator, round(Point.est, digits = 3), round(ST.error, digits = 3), Confinterval, stringsAsFactors = F)
  colnames(summary.table) <- c("Estimator", "point.est", "St.error", paste((1 - alpha) * 100, "% Confidence Interval", sep = ""))
  return(summary.table)
}
