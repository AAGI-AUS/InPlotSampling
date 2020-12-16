#' Computes the estimator for JPS data
#'
#' @param data The data to use for estimation
#' @param setsize The set size
#' @param replace Logical (default `TRUE`). Sample with replacement?
#' @param model
#' @param N The population size.
#' @param alpha The significance level.
#'
#' @return
#' @keywords internal
#'
JPSEF <- function(data, setsize, replace = TRUE, model, N, alpha) {

  K <- ncol(data) - 1

  # if (is.null(N)) {
  #   stop("Population size N must be provided for sampling without replacement")
  # }
  if (!replace && is.null(N)) {
    stop("Population size N must be provided for sampling without replacement")
  }

  n <- nrow(data)
  Coefn <- CoefF(setsize, n)
  if (K == 1) {
    Coef.del1 <- NULL
  }
  else {
    Coef.del1 <- CoefF(setsize, n - 1)
  }

  if (!replace) {
    coef1D2 <- Coefn[1]
    coef2D2 <- 1 / (setsize * (setsize - 1)) + Coefn[3] + Coefn[2] - (Coefn[2] + 1 / setsize^2) * setsize / (setsize - 1)
    coef3D2 <- Coefn[2] - 1 / (N - 1) * (1 / setsize - (Coefn[1] + 1 / setsize^2))
    CoefD <- c(coef1D2, coef2D2, coef3D2)

    coef1D2.del <- Coef.del1[1]
    coef2D2.del <- 1 / (setsize * (setsize - 1)) + Coef.del1[3] + Coef.del1[2] - (Coef.del1[2] + 1 / setsize^2) * setsize / (setsize - 1)
    coef3D2.del <- Coef.del1[2] - 1 / (N - 1) * (1 / setsize - (Coef.del1[1] + 1 / setsize^2))
    Coef.Del <- c(coef1D2.del, coef2D2.del, coef3D2.del)

    fc <- 1 - n / N # finite population correction factor
  }
  else {
    CoefD <- Coefn
    Coef.Del <- Coef.del1

    fc <- 1 # finite population correction factor
  }

  if (model == 1) {
    CoefD <- Coefn
    Coef.Del <- Coef.del1
  }

  ########################################### 3333333
  #  if there is only one ranker
  if (K == 1) {
    EST.EqSd <- JPSEDF(data[, -1], Y = data[, 1], setsize = setsize, N = N, Coef = CoefD, CoefDel = Coef.Del, replace = replace, model = model, K)
    # print(EST.EqSd)
    Estimator <- c("JPS", "SRS")
    Estimate <- round(c(EST.EqSd[1], mean(data[, 1])), digits = 3)
    St.error <- round(c(sqrt(EST.EqSd[2]), sd(data[, 1]) / sqrt(n)), digits = 3)
    Lower.Limit <- round(Estimate - qt(1 - alpha / 2, n - 1) * St.error, digits = 3)
    Upper.Limit <- round(Estimate + qt(1 - alpha / 2, n - 1) * St.error, digits = 3)
    CI <- paste(Lower.Limit, Upper.Limit, sep = ",")
    Summary.return <- data.frame(Estimator, Estimate, St.error, CI)
    colnames(Summary.return) <- c("Estimator", "Estimate", "Standard Error", paste((1 - alpha) * 100, "% Confidence intervals", sep = ""))
    return(Summary.return)
  }


  EST.EqSd <- apply(data[, -1], 2, JPSEDF, Y = data[, 1], setsize = setsize, N = N, Coef = CoefD, CoefDel = Coef.Del, replace = replace, model = model, K)

  JPSE.Fulln <- EST.EqSd[1, ] #  JPS mean estimate for each ranker using all n data
  JPSV.Fulln <- EST.EqSd[2, ] # Variance estiamte of JPS mean  for each ranker using all n data
  V.Sd <- EST.EqSd[c(3:(n + 2)), ] #  Variance of JPS mean estiamte for each ranker when the i-th observation is deleted
  # the i-th row corresponds to variance estimate when the i-th observation
  # is deleted
  EST.Equal <- EST.EqSd[-c(1:(n + 2)), ] #  JPS mean estiamte for each ranker when the i-th observation is deleted
  # the i-th row corresponds to JPS estimate when the i-th observation
  # is deleted
  # if (replace) fc <- 1 else fc <- 1 - n / N # finite population correction factor
  JPSE.Kn <- sum((1 / JPSV.Fulln) * JPSE.Fulln) / sum(1 / JPSV.Fulln) #  varince weighted JPS estimate

  Prec.Weight <- t(apply(V.Sd, 1, function(u) {
    (1 / u) / sum(1 / u)
  })) # variance  weights
  JACk.Repl.Sd <- diag(Prec.Weight %*% t(EST.Equal)) # Jackknife feplicate of variance weighted  estimator
  Jack.VEST.Sd <- fc * (n - 1) * var(JACk.Repl.Sd) * ((n - 1) / n)^2 # Jackknife varince estiamte of variabce weighted JPS estimator
  JPS1 <- JPSE.Fulln[1] # JPS estimator based on best ranker
  JPS1.VEST <- JPSV.Fulln[1] # Variance estimate of JPS estimator of best ranker
  EqWeight.Est <- mean(JPSE.Fulln) # Equal weight estimator
  Jack.Repl.EqWeight <- apply(EST.Equal, 1, mean) # Jackknife replicates for equalk weight JPS estimator
  Jack.EST.EqWeight <- fc * (n - 1) * var(Jack.Repl.EqWeight) * ((n - 1) / n)^2 # Jackknife variance estiamte  for equal weight JPS estimator



  #################################### 33
  # agreement weight estimator
  AW <- data[, -1] # Ranks
  AW <- t(apply(data.frame(data[, -1]), 1, WEIGHTF, setsize = setsize)) # agreemeent weights
  eff.SS <- apply(AW, 2, sum)
  Crosprod <- data[, 1] %*% AW
  EST.Agree.Weight <- mean(Crosprod[eff.SS > 0] / eff.SS[eff.SS > 0]) # JPS agreement weight estiamtor
  AWY <- cbind(data[, 1], AW)
  Jack.Repl.AWi <- apply(matrix(1:n, ncol = 1), 1, FWDel1, AWY = AWY) # Aggrement weight estimator
  # when the i-th observation is deleted
  Jack.Est.AW <- fc * (n - 1) * var(Jack.Repl.AWi) * ((n - 1) / n)^2 # Jackknife variance estiamte  for aggreement weight JPS estimator
  ##############################################################
  # print(cbind(Jack.Repl.EqWeight,JACk.Repl.Sd,Jack.Repl.AWi))


  Estimate.est <- c(EqWeight.Est, JPSE.Kn, EST.Agree.Weight, JPS1, mean(data[, 1]))
  Variance.est <- c(Jack.EST.EqWeight, Jack.VEST.Sd, Jack.Est.AW, JPS1.VEST, var(data[, 1]) / n)

  min.ind <- which(Variance.est == min(Variance.est)) # Find the estimator having minimum variance
  # among four estimator
  Variance.est <- c(Variance.est, Variance.est[min.ind]) # bind the variance of minimum
  # variance estimator
  St.error <- sqrt(Variance.est)
  Estimate.est <- c(Estimate.est, Estimate.est[min.ind]) # bind the minimum variance estimator
  Lower.Limit <- Estimate.est - qt(1 - alpha / 2, n - 1) * sqrt(Variance.est)
  Upper.Limit <- Estimate.est + qt(1 - alpha / 2, n - 1) * sqrt(Variance.est)
  Lower.Limit <- round(Lower.Limit, digits = 3)
  Upper.Limit <- round(Upper.Limit, digits = 3)
  # test=(LL-popmean)*(UL-popmean)
  # coef=1*(test <0)
  CI <- paste(Lower.Limit, Upper.Limit, sep = ",")
  Estimator <- c("UnWeighted", "Sd.Weighted", "Aggregate Weight", "JPS Estimate", "SRS estimate", "Minimum")
  # Summary.ret=data.frame(Estimator,Estimate.est,Variance.est,Lower.Limit,Upper.Limit)
  Summary.ret <- data.frame(Estimator, round(Estimate.est, digits = 3), round(St.error, digits = 3), CI)
  colnames(Summary.ret) <- c("Estimator", "Estimate", "Standard Error", paste((1 - alpha) * 100, "% Confidence intervals", sep = ""))
  #  Summary.ret=round(Summary.ret,digits=3)
  # print(Summary.ret)
  return(Summary.ret)
}
