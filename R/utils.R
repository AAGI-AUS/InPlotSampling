is_positive_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  is_wholenumber(x, tol) && x > 0
}

is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

verify_rss_params <- function(pop, n, H, K) {
  pop_dimension <- dim(pop)
  if (length(pop_dimension) != 2) {
    stop("`pop` must be a 2-dimension matrix-like object.")
  }

  if (pop_dimension[[2]] < 2) {
    stop("`pop` must have at least 2 columns.")
  }

  if (!is_positive_wholenumber(n)) {
    stop("`n` must be a positive whole number.")
  }

  if (!is_positive_wholenumber(H)) {
    stop("`H` must be a positive whole number.")
  }

  if (!is_positive_wholenumber(K)) {
    stop("`K` must be a positive whole number.")
  }

  if (pop_dimension[[1]] < n) {
    stop("`pop` must have at least `n` rows.")
  }

  if (n < H) {
    stop("`n` must >= `H`.")
  }

  if (n %% H != 0) {
    stop("`n` must be a multiple of `H`.")
  }
}
