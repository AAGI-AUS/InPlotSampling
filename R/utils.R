is_boolean <- function(v) {
  return(v %in% c(TRUE, FALSE))
}
is_positive_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  is_wholenumber(x, tol) && x > 0
}

is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

verify_rss_params <- function(pop, n, H, K) {
  verify_positive_whole_number(n, H, K)
  pop_dimension <- dim(pop)

  if (length(pop_dimension) != 2) {
    stop("`pop` must be a 2-dimension matrix-like object.")
  }

  if (pop_dimension[[1]] < n) {
    stop("`pop` must have at least `n` rows.")
  }

  pop_dimension <- dim(pop)

  if (pop_dimension[[2]] < 2) {
    stop("`pop` must have at least 2 columns.")
  }

  if (n < H) {
    stop("`n` must >= `H`.")
  }

  if (n %% H != 0) {
    stop("`n` must be a multiple of `H`.")
  }
}

verify_rssnrf_params <- function(pop, n, H, K) {
  verify_rss_params(pop, n, H, K)

  n_population <- dim(pop)[[1]]
  if (n_population < n * H) {
    stop("The number of population must be at least `nH`.")
  }
}

verify_jps_params <- function(pop, n, H, tau, K, with_replacement) {
  verify_positive_whole_number(n, H, K)
  verify_boolean(with_replacement)

  if (n < H) {
    stop("`n` must >= `H`.")
  }


  if (length(tau) != K) {
    stop("The length of `tau` must equal to `K`.")
  }

  n_population <- length(pop)
  if (!with_replacement) {
    if (n_population < n * H) {
      stop("The number of population must be at least `nH`.")
    }
  }
}

verify_boolean <- function(..., var_names = NULL) {
  verify_data_type(is_boolean, "boolean-like object", var_names, ...)
}

verify_positive_whole_number <- function(..., var_names = NULL) {
  verify_data_type(is_positive_wholenumber, "positive whole number", var_names, ...)
}

verify_data_type <- function(verify_func, data_type, var_names = NULL, ...) {
  if (is.null(var_names)) {
    var_names <- get_var_names(...)
  }

  args <- list(...)
  for (i in seq_along(args)) {
    v <- args[[i]]

    if (!verify_func(v)) {
      data_type_error(var_names[[i]], data_type)
    }
  }
}

get_var_names <- function(...) {
  raw_names <- deparse(substitute(list(...)))
  names <- substr(raw_names, 6, nchar(raw_names) - 1)
  return(strsplit(names, ", ")[[1]])
}

data_type_error <- function(var_name, expected_data_type) {
  stop(paste0("`", var_name, "` must be a ", expected_data_type, "."))
}
