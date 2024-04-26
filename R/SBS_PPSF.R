# Y            : Sample response values
# sample.matrix: Sample matrix, contains four columns
#               column 1: Halton numbers
#               Column 2:  X1 coordinates
#               Column 3:  X2 coordinates
#               column 4:  Size measurements
#               Column 5:  Inclusion probabilities
# Infer         : Logical expression ff FALSE only the samle is generated
#              : and Head map of the generated sample units  are provided
# D             : Bootstrap sample size
# alpha        : 1- alpha is the coverage probability of  the confidence interval
#' Generate spatially balanced sampling on the population provided.
#'
#' @param pop Population data frame to be sampled with 4 columns.
#' 1. Halton numbers
#' 2. X1-coordinate of population unit
#' 3. X2-coordinate of population unit
#' 4. Size measurements of population units
#' @param n Sample sizes (SBS sample size, PPS sample size).
#' @param H Set size for each raking group.
#' @param K Number of rankers.
#'
#' @return A matrix with ranks from each ranker.
#'
sbs_pps_estimate <- function(pop, n, Y = NULL, sample_matrix = NULL, infer = FALSE, D = 100, alpha = 0.05) {
  n_population <- dim(pop)[1]
  minimum_size <- min(pop[, 4])

  # TODO: verify parameters
  if (minimum_size == 0) {
    print("Some size measurement are zero")
    stop()
  }
  if (minimum_size < 0) {
    ("Some size measurements are negative")
    stop()
  }

  #  1- population mean
  #  2- estimate the variance of the population mean estimator
  # 3- construct a confidence interval for population mean
  #
  if (is.null(Y)) {
    print("Response values for sample units must be provided")
    return
  }
  # include response measurements Y in sample.matrix
  #  sample.matrix=cbind(sample.matrix[,c(1,2,3)],Y,sample.matrix[,c(4,5,6)])
  sample_matrix <- cbind(sample_matrix, Y)
  if (n[1] != 0) { # If SBS sample size n1 is not zero,
    # compute augmented SBS-pps sample estimator
    replicated <- duplicated(sample_matrix[, 1]) # remove replicated sample units
    sample_matrix <- sample_matrix[!replicated, ] # to prevent over estimation
    YY <- sample_matrix[, 7]
    SBS1_pps1_est <- round(sum(sample_matrix[, 7] / sample_matrix[, 6]) / n_population, digit = 3) # point estimate of population mean

    EMPIR_Pop <- EmpirPop_partitionF(sample_matrix[, 1], pop, YY) # imputed empirical population
    PI_empir <- calculate_inclusion_prob(EMPIR_Pop[, 3], n) # First order inclusion prob of                                               # empirical population using
    # imputed size variable
    EMPIR_Pop <- data.frame(EMPIR_Pop, PI_empir) # combine empirical population
    # with first order inclusion probabilities
    Boot_var_est <- BootF(EMPIR_Pop, n, D) # compute bootstrap variance estimate
    Boot.var.est <- round(Boot_var_est, digit = 3) # roudns to three significant digits
    Var.est.sbs_pps <- Boot.var.est # Bootstrap variance estimate of population mean estimator
  } else { # Compute the variance of pps sample
    YS <- sample_matrix[, 7]
    WS <- sample_matrix[, 5]
    SBS1_pps1_est <- mean(YS / WS) / n_population # pps sample
    ################## PPS sample variance estimator
    Var.est.sbs_pps <- sum((YS / WS - mean(YS / WS))^2) / ((length(YS) - 1) * length(YS) * n_population^2)
    VAR_estimator <- "PPS" # identify the type of variance estimator
  }

  ########################################################################
  ## constructs (1-alpha)100 % confidence interval
  Lower.Limit <- SBS1_pps1_est - qt(1 - alpha / 2, sum(n) - 1) * sqrt(Var.est.sbs_pps)
  Upper.Limit <- SBS1_pps1_est + qt(1 - alpha / 2, sum(n) - 1) * sqrt(Var.est.sbs_pps)
  Lower.Limit <- round(Lower.Limit, digits = 3)
  Upper.Limit <- round(Upper.Limit, digits = 3)
  CI <- paste(Lower.Limit, Upper.Limit, sep = ",") # Concatenate the lower and upper limit
  # summary of returned point estimate, variance estimate and con fidence interval
  Summary.ret <- data.frame(n[1], n[2], SBS1_pps1_est, sqrt(Var.est.sbs_pps), CI)
  colnames(Summary.ret) <- c("n1", "n2", "Estimate", "St.error", paste((1 - alpha) * 100, "% Confidence intervals", sep = ""))
  rownames(Summary.ret) <- NULL
  return(Summary.ret)
}
