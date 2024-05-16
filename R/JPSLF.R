#' Judgement Post Stratification List Function
#'
#' @param data Data to use to estimate.
#' @param set_size Number of samples which are ranked in each set.
#' @param replace Logical. Are samples drawn with replacement?
#' @param population_size Population size of the population which `data` is taken from.
#' @param model Whether to use design-based approach or super-population (model based) approach
#' @param estimator Which type of estimator to return. Default is "SD Weighted". Other options are "Unweighted", "Aggregate Weight", "JPS Estimate", "SRS estimate", "Minimum"
#'
#' @return
#' @keywords internal
#'
JPSLF <- function(data, set_size, replace, population_size, model, estimator = "SD Weighted") {
  # JPS using List function
  # Set size provided by user, if replace is FALSE, then set_size*samplesize <= pop size
  # If replace = TRUE, no restriction on set size.
  # Check if one ranker - just return JPS estimate
  # multiple rankers: return sd.weighted by default, optionally return all estimates
  # model is switch for design-based or model based (super population)

  # model <- match.arg(type)

  estimator <- match.arg(estimator,
    arg = c(
      "SD Weighted",
      "Unweighted",
      "Aggregate Weight",
      "JPS Estimate",
      "SRS estimate",
      "Minimum"
    ),
    several.ok = TRUE
  )

  # Check arguments
  # if(!replace & set_size*nrow(data) > population_size) {
  #     stop("population_size must not be less than set_size*nrow(data)")
  # }

  # nK <- dim(data)
  n <- dim(data)[1]
  K <- dim(data)[2] - 1
  Coefn <- calculate_coefficients(set_size, n)
  #######################################
  # Compute coefficient to go into JPSED0G
  if (replace) {
    if (is.null(population_size)) {
      print("Population size population_size must be provided for without replacement sampling")
    }
    coef1D2 <- Coefn[1]
    coef2D2 <- 1 / (set_size * (set_size - 1)) + Coefn[3] + Coefn[2] - (Coefn[2] + 1 / set_size^2) * set_size / (set_size - 1)
    coef3D2 <- Coefn[2] - 1 / (population_size - 1) * (1 / set_size - (Coefn[1] + 1 / set_size^2))
    CoefD <- c(coef1D2, coef2D2, coef3D2)
  } else {
    CoefD <- Coefn
  }
  if (model == 1) CoefD <- Coefn
  ############################################
  if (K == 1) {
    #  print(K)
    JPSE.V <- JPSED0F(data[, 2], data[, 1], set_size, CoefD, population_size, replace, model) # single ranking method estiamte
    #  print(JPSE.V)
    Estimator <- "JPS"
    Point.Est <- JPSE.V[1]
    Variance.Point <- JPSE.V[2]
    Lower.Limit <- Point.Est - qt(1 - alpha / 2, n - 1) * sqrt(Variance.Point)
    Upper.Limit <- Point.Est + qt(1 - alpha / 2, n - 1) * sqrt(Variance.Point)
    Summary.return <- data.frame(Estimator, Point.Est, Variance.Point, Lower.Limit, Upper.Limit)
    return(Summary.return)
  }
  JPSE.E <- ListF(data, set_size, replace, population_size, model, Coefn) # estimate based on combined ranking method
  ################
  # create list of data frames by deleting each row
  # this is used for Jackknife variance estimate
  DataL <- vector("list", n)
  for (i in (1:n)) {
    DataL[[i]] <- data[-i, ]
  }
  Coefn1 <- calculate_coefficients(set_size, n - 1)
  ###########################################
  ### This line is the slow part
  delet1 <- lapply(DataL, ListF, set_size, replace, population_size, model, Coefn1) # Compute n different combined estimate
  # by deleting one observation at a time


  JPSE.V <- JPSED0F(data[, 2], data[, 1], set_size, CoefD, population_size, replace, model) # single ranking method estiamte
  # print(JPSE.V)

  delet1M <- do.call(rbind, delet1) # Convert list to matrix
  delet1M <- rbind(JPSE.E, delet1M)
  fc <- 1
  if (replace == 1) {
    fc <- (1 - n / population_size)
  }
  JackV <- fc * apply(delet1M, 2, JACKVF) # Jackknife variance estimate of the combined estimator
  Variance.est <- c(JackV, JPSE.V[2]) # bind the variance of JPS estimator based on single ranking method
  Estimate.est <- c(JPSE.E, JPSE.V[1]) # bind the  JPS estimator based on single ranking method
  min.ind <- which(Variance.est == min(Variance.est)) # Find the estimator having minimum variance
  # among four estimator
  Variance.est <- c(Variance.est, var(data[, 1]) / n, Variance.est[min.ind]) # bind the variance of minimum
  # variance estimator
  St.error <- sqrt(Variance.est)
  Estimate.est <- c(Estimate.est, mean(data[, 1]), Estimate.est[min.ind]) # bind the minimum variance estiamtor
  Lower.Limit <- Estimate.est - qt(1 - alpha / 2, n - 1) * sqrt(Variance.est)
  Upper.Limit <- Estimate.est + qt(1 - alpha / 2, n - 1) * sqrt(Variance.est)
  Lower.Limit <- round(Lower.Limit, digits = 3)
  Upper.Limit <- round(Upper.Limit, digits = 3)
  # test=(LL-popmean)*(UL-popmean)
  # coef=1*(test <0)
  CI <- paste(Lower.Limit, Upper.Limit, sep = ",")
  Estimator <- c("UnWeighted", "Sd.Weighted", "Aggregate Weight", "JPS Estimate", "SRS estimate", "Minimum")
  # Summary.ret=data.frame(Estimator,Estimate.est,Variance.est,Lower.Limit,Upper.Limit)
  Summary.ret <- data.frame(Estimator, round(Estimate.est, digits = 3), round(St.error, digits = 3), CI, stringsAsFactors = F)
  colnames(Summary.ret) <- c("Estimator", "Estimate", "Standard Error", paste((1 - alpha) * 100, "% Confidence intervals", sep = ""))
  #  Summary.ret=round(Summary.ret,digits=3)
  return(Summary.ret)
}
