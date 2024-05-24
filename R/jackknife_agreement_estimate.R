# TODO: add doc
#' @keywords internal
#'
jackknife_agreement_estimate <- function(ranks, y, set_size, pop_size, fc) {
  n <- nrow(ranks)

  agreement_weights <- t(apply(data.frame(ranks), 1, calculate_agreement_weights, set_size))
  aw_sum <- apply(agreement_weights, 2, sum)
  cross_product <- y %*% agreement_weights
  agreement_mean <- mean(cross_product[aw_sum > 0] / aw_sum[aw_sum > 0])

  awy <- cbind(y, agreement_weights)
  jackknife_mean <- apply(matrix(1:n, ncol = 1), 1, FWDel1, AWY = awy)
  jackknife_var <- fc * (n - 1) * var(jackknife_mean) * ((n - 1) / n)^2

  return(list(agreement_mean = agreement_mean, jackknife_var = jackknife_var))
}
