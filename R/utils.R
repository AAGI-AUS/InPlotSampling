default_tolerance <- .Machine$double.eps^0.5

get_interval <- function(sample_mean, sample_size, std_error, alpha, round_digits = NULL) {
  qt_term <- qt(1 - alpha / 2, sample_size - 1)
  lower_bound <- sample_mean - qt_term * std_error
  upper_bound <- sample_mean + qt_term * std_error

  if (!is.null(round_digits)) {
    lower_bound <- round(lower_bound, digits = round_digits)
    upper_bound <- round(upper_bound, digits = round_digits)
  }

  return(list(lower_bound = lower_bound, upper_bound = upper_bound))
}

is_between_ <- function(lower, upper, lower_exclude = FALSE, upper_exclude = FALSE) {
  return(function(x) {
    is_between <- is.numeric(x)

    if (lower_exclude) {
      is_between <- is_between && x > lower
    } else {
      is_between <- is_between && x >= lower
    }

    if (upper_exclude) {
      is_between <- is_between && x < upper
    } else {
      is_between <- is_between && x <= upper
    }

    return(is_between)
  })
}

is_boolean <- function(v) {
  return(must_be(v, c(TRUE, FALSE)))
}

is_matrix_like <- function(n_dimensions, n_rows, n_cols) {
  return(function(v) {
    dimension <- dim(v)
    is_valid <- length(dimension) == n_dimensions
    if (!is.null(n_rows)) {
      is_valid <- is_valid && dimension[[1]] >= n_rows
    }
    if (!is.null(n_cols)) {
      is_valid <- is_valid && dimension[[2]] >= n_cols
    }

    return(is_valid)
  })
}

is_non_negative_whole_number <- function(x, tol = default_tolerance) {
  return(is_whole_number(x, tol) && x >= 0)
}

is_positive_whole_number <- function(x, tol = default_tolerance) {
  return(is_whole_number(x, tol) && x > 0)
}

is_whole_number <- function(x, tol = default_tolerance) {
  if (!is.numeric(x)) {
    return(FALSE)
  }

  return(abs(x - round(x)) < tol)
}

must_be <- function(x, valid_values) {
  return(must_be_(valid_values)(x))
}

must_be_ <- function(valid_values) {
  return(function(x) {
    for (v in valid_values) {
      if (identical(x, v)) {
        return(TRUE)
      }
    }

    return(FALSE)
  })
}

verify_rss_jps_estimate_params <- function(data, set_size, method, confidence, replace, model_based, pop_size) {
  verify_positive_whole_number(set_size)
  verify_boolean(replace, model_based)
  verify_between(confidence, lower = 0, upper = 1)

  valid_methods <- c("JPS", "RSS")
  verify_must_be(method, valid_values = valid_methods)

  if (!replace) {
    if (!is.numeric(pop_size)) {
      stop("A numeric population size `pop_size` must be provided when sampling without replacement")
    } else if (pop_size < nrow(data) * set_size || pop_size <= 0) {
      stop("`pop_size` must be positive and greater or equal to `data x set_size`")
    }
  }

  if (model_based && !is.numeric(pop_size)) {
    stop("The population size `pop_size` must be provided for super-population model")
  }
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

verify_rss_wo_replace_params <- function(pop, n, H, K) {
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

verify_between <- function(
    ..., lower = -Inf, upper = Inf, lower_exclude = FALSE, upper_exclude = FALSE, var_names = NULL) {
  if (lower != -Inf && upper != Inf) {
    object_type <- paste0("between ", lower)
    if (lower_exclude) {
      object_type <- paste0(object_type, " (exclusive)")
    }

    object_type <- paste0(object_type, " and ", upper)
    if (upper_exclude) {
      object_type <- paste0(object_type, " (exclusive)")
    }

    if (!lower_exclude && !upper_exclude) {
      object_type <- paste0("inclusively ", object_type)
    }
  } else if (upper == Inf) {
    if (lower_exclude) {
      object_type <- paste0("greater than ", lower)
    } else {
      object_type <- paste0("at least ", lower)
    }
  } else if (lower == -Inf) {
    if (upper_exclude) {
      object_type <- paste0("less than ", upper)
    } else {
      object_type <- paste0("at most ", upper)
    }
  }
  object_type <- paste0(object_type, ".")

  verify_data_type(is_between_(lower, upper), object_type, var_names, ...)
}

verify_boolean <- function(..., var_names = NULL) {
  verify_data_type(is_boolean, "a boolean", var_names, ...)
}

verify_matrix_like <- function(..., n_dimensions, n_rows = NULL, n_cols = NULL, var_names = NULL) {
  matrix_like <- paste0("a ", n_dimensions, "-dimension matrix-like object")
  if (!is.null(n_rows)) {
    matrix_like <- paste0(matrix_like, " with at least ", n_rows, " rows")
  }
  if (!is.null(n_cols)) {
    if (!is.null(n_rows)) {
      matrix_like <- paste0(matrix_like, " and ")
    } else {
      matrix_like <- paste0(matrix_like, " with at least ")
    }
    matrix_like <- paste0(matrix_like, n_cols, " columns")
  }
  matrix_like <- paste0(matrix_like, ".")

  verify_data_type(is_matrix_like(n_dimensions, n_rows, n_cols), matrix_like, var_names, ...)
}

verify_non_negative_whole <- function(..., var_names = NULL) {
  verify_data_type(is_non_negative_whole_number, "a non-negative whole number", var_names, ...)
}

verify_positive_whole_number <- function(..., var_names = NULL) {
  verify_data_type(is_positive_whole_number, "a positive whole number", var_names, ...)
}

verify_must_be <- function(..., valid_values, var_names = NULL) {
  literal_values <- get_literal_values(valid_values)
  verify_data_type(must_be_(valid_values), literal_values, var_names, ...)
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

get_literal_values <- function(values) {
  n_values <- length(values)
  literal_values <- literal(values[[1]])
  if (n_values == 2) {
    return(paste0(literal_values, " or ", literal(values[[2]])))
  }

  for (i in 2:n_values) {
    if (i < n_values) {
      literal_values <- paste0(literal_values, ", ", literal(values[[i]]))
    } else {
      literal_values <- paste0(literal_values, ", or ", literal(values[[i]]))
    }
  }
}

get_var_names <- function(...) {
  raw_names <- deparse(substitute(list(...)))
  names <- substr(raw_names, 6, nchar(raw_names) - 1)
  return(strsplit(names, ", ")[[1]])
}

data_type_error <- function(var_name, expected_data_type) {
  stop(paste0("`", var_name, "` must be ", expected_data_type, "."))
}

literal <- function(v) {
  if (is.character(v)) {
    return(paste0('`"', v, '"`'))
  }

  return(paste0("`", v, "`"))
}
