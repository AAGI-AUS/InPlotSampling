default_tolerance <- .Machine$double.eps^0.5

is_between <- function(x, lower, upper) {
  return(is_between_(lower, upper)(x))
}

is_between_ <- function(lower, upper) {
  return(function(x) {
    return(is.numeric(x) && x >= lower && x <= upper)
  })
}

is_boolean <- function(v) {
  return(must_be(v, c(TRUE, FALSE)))
}

is_positive_wholenumber <- function(x, tol = default_tolerance) {
  return(is_wholenumber(x, tol) && x > 0)
}

is_wholenumber <- function(x, tol = default_tolerance) {
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

verify_one_sample_params <- function(data, set_size, method, confidence, replace, model, pop_size) {
  verify_positive_whole_number(set_size)
  verify_boolean(replace)
  verify_between(confidence, lower = 0, upper = 1)

  valid_methods <- c("JPS", "RSS")
  verify_must_be(method, valid_values = valid_methods)

  valid_models <- c(0, 1)
  verify_must_be(model, valid_values = valid_models)

  if (!replace) {
    if (!is.numeric(pop_size)) {
      stop("A numeric population size `pop_size` must be provided when sampling without replacement")
    } else if (pop_size < nrow(data) * set_size || pop_size <= 0) {
      stop("`pop_size` must be positive and greater or equal to `data x set_size`")
    }
  }

  if (model == 1 && !is.numeric(pop_size)) {
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

verify_between <- function(..., lower, upper, var_names = NULL) {
  object_type <- paste0("inclusively between ", lower, " and ", upper)
  verify_data_type(is_between_(lower, upper), object_type, var_names, ...)
}

verify_boolean <- function(..., var_names = NULL) {
  verify_data_type(is_boolean, "a boolean", var_names, ...)
}

verify_positive_whole_number <- function(..., var_names = NULL) {
  verify_data_type(is_positive_wholenumber, "a positive whole number", var_names, ...)
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
