rinvgamma <- function (n,
                       shape,
                       rate = 1,
                       scale = 1 / rate)
{
  if (missing(rate) && !missing(scale))
    rate <- 1 / scale
  1 / stats::rgamma(n, shape, rate)
}

lm_to_obj <- function(lm_obj) {
  # extract model parameters
  beta_hat <- stats::coef(lm_obj)

  if (anyNA(beta_hat)) {
    stop(
      "The model is rank deficient: the coefficients ",
      paste(sQuote(names(beta_hat)[is.na(beta_hat)]), collapse = ", "),
      " could not be estimated. Drop the aliased terms and refit before simulating.",
      call. = FALSE
    )
  }

  summary_obj <- summary(lm_obj)
  sigma_hat <- summary_obj$sigma
  unscaled_vcov <- summary_obj$cov.unscaled
  n <- length(stats::residuals(lm_obj))
  k <- length(beta_hat)

  return(list(
    beta_hat = beta_hat,
    unscaled_vcov = unscaled_vcov,
    sigma_hat = sigma_hat,
    n = n,
    k = k
  ))
}

# Validate a user supplied list standing in for an lm object. Documented under
# @param input_obj, but until now nothing assigned it, so obj$beta_hat failed.
check_obj <- function(obj) {
  if (!is.list(obj)) {
    stop(
      "input_obj must either be an lm object or a list with the entries ",
      "beta_hat, unscaled_vcov, sigma_hat, n and k.",
      call. = FALSE
    )
  }

  required <- c("beta_hat", "unscaled_vcov", "sigma_hat", "n", "k")
  missing_fields <- setdiff(required, names(obj))
  if (length(missing_fields) > 0) {
    stop(
      "The list passed as input_obj is missing the entries: ",
      paste(missing_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  obj$beta_hat <- as.vector(obj$beta_hat)
  obj$unscaled_vcov <- as.matrix(obj$unscaled_vcov)
  obj$k <- as.integer(obj$k)
  obj$n <- as.integer(obj$n)

  if (length(obj$beta_hat) != obj$k) {
    stop("length(beta_hat) must equal k.", call. = FALSE)
  }
  if (any(dim(obj$unscaled_vcov) != obj$k)) {
    stop("unscaled_vcov must be a k by k matrix.", call. = FALSE)
  }
  if (length(obj$sigma_hat) != 1L || !is.finite(obj$sigma_hat) || obj$sigma_hat <= 0) {
    stop("sigma_hat must be a single positive number.", call. = FALSE)
  }
  if (obj$n <= obj$k) {
    stop("n must be larger than k.", call. = FALSE)
  }

  obj
}

# Upper triangular factor R with t(R) %*% R == V, falling back to a symmetric
# eigen factorisation when V is only positive semi-definite.
vcov_factor <- function(V) {
  f <- tryCatch(chol(V), error = function(e) NULL)
  if (!is.null(f)) {
    return(f)
  }

  e <- eigen(V, symmetric = TRUE)
  values <- pmax(e$values, 0)
  t(e$vectors %*% diag(sqrt(values), nrow = length(values)))
}

sim_param <- function(nsim,
                      beta_hat,
                      unscaled_vcov,
                      sigma_hat,
                      n,
                      k) {
  sigma2_sim <-
    rinvgamma(nsim,
              shape = (n - k) / 2,
              scale = 2 / (sigma_hat ^ 2 * (n - k)))

  # Every draw shares the same correlation structure and differs only in scale,
  # so the k by k matrix is factored once instead of once per draw.
  R <- vcov_factor(unscaled_vcov)

  Z <- matrix(stats::rnorm(nsim * k), nrow = nsim, ncol = k)
  beta_sim <- Z %*% R * sqrt(sigma2_sim)
  beta_sim <- beta_sim + rep(beta_hat, each = nsim)
  colnames(beta_sim) <- names(beta_hat)

  return(list(betas = beta_sim, sigma = sigma2_sim))
}

sim_logy <- function(nsim, E_log_Y, sigma, exponentiate = TRUE) {
  logY_sim <- stats::rnorm(nsim,
                           E_log_Y,
                           sigma)
  if (exponentiate == TRUE) {
    return(exp(logY_sim))
  }
  if (exponentiate == FALSE) {
    return(logY_sim)
  }
}

# ---------------------------------------------------------------------------
# Scenario construction
# ---------------------------------------------------------------------------

# The data an lm was fitted on, with the variables on their *original* scale.
# input_obj$model holds transformed columns ("log(speed)"), which is why the old
# colMeans() default could not be fed back through model.frame().
model_data <- function(lm_obj) {
  env <- environment(stats::formula(lm_obj))
  if (is.null(env)) {
    env <- parent.frame()
  }

  data <- NULL
  if (!is.null(lm_obj$call$data)) {
    data <- tryCatch(eval(lm_obj$call$data, env), error = function(e) NULL)
  }
  if (is.null(data)) {
    data <- env
  }

  raw <- tryCatch(
    stats::get_all_vars(stats::formula(lm_obj), data = data),
    error = function(e) NULL
  )

  if (is.null(raw)) {
    stop(
      "The data the model was fitted on could not be recovered, so no default ",
      "scenario can be built. Supply a scenario explicitly, or pass a model ",
      "matrix via X.",
      call. = FALSE
    )
  }

  # get_all_vars() returns the full data; keep the rows that entered the fit.
  used <- rownames(stats::model.frame(lm_obj))
  if (!is.null(used) && !is.null(rownames(raw)) && all(used %in% rownames(raw))) {
    raw <- raw[used, , drop = FALSE]
  }

  raw
}

# Variables that enter a log() anywhere in an expression. Used to reject
# non-positive scenario values before model.frame() produces NaNs.
vars_inside_log <- function(expr) {
  if (!is.call(expr)) {
    return(character())
  }

  fn <- as.character(expr[[1L]])[1L]
  if (fn %in% c("log", "log2", "log10")) {
    return(all.vars(expr))
  }

  unlist(lapply(as.list(expr)[-1L], vars_inside_log))
}

# The default scenario: numerics at their mean, factors at their reference
# level. Replaces colMeans(input_obj$model), which mangled transformed column
# names and errored outright on factors.
default_scenario <- function(lm_obj, raw, predictors) {
  scen <- list()

  for (v in predictors) {
    x <- raw[[v]]

    if (is.factor(x) || is.character(x) || !is.null(lm_obj$xlevels[[v]])) {
      levs <- lm_obj$xlevels[[v]]
      if (is.null(levs)) {
        levs <- levels(as.factor(x))
      }
      scen[[v]] <- factor(levs[1L], levels = levs)
    } else if (is.logical(x)) {
      scen[[v]] <- mean(x, na.rm = TRUE) >= 0.5
    } else {
      scen[[v]] <- mean(x, na.rm = TRUE)
    }
  }

  scen
}

# Turn a user scenario (or the default) into a data frame of scenario rows on
# the original scale of the variables.
build_scenario_frame <- function(lm_obj, scenario, verbose = TRUE) {
  Terms <- stats::delete.response(stats::terms(lm_obj))
  predictors <- all.vars(stats::formula(Terms))

  raw <- model_data(lm_obj)

  if (is.null(scenario)) {
    if (verbose) {
      message(
        "No scenario provided: numeric variables were set to their mean and ",
        "factors to their reference level."
      )
    }
    scenario <- default_scenario(lm_obj, raw, predictors)
  } else {
    if (!is.list(scenario) || is.null(names(scenario)) || any(names(scenario) == "")) {
      stop("scenario must be a named list.", call. = FALSE)
    }

    unknown <- setdiff(names(scenario), predictors)
    if (length(unknown) > 0) {
      stop(
        "The scenario names ",
        paste(sQuote(unknown), collapse = ", "),
        " are not variables in the model. Available variables: ",
        paste(sQuote(predictors), collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    lens <- lengths(scenario)
    n_scen <- max(lens)
    if (any(lens != 1L & lens != n_scen)) {
      stop(
        "All scenario entries must have length 1 or the same length. Got: ",
        paste(paste0(names(scenario), " (", lens, ")"), collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    # Fill the variables the user did not name from the defaults.
    filled <- setdiff(predictors, names(scenario))
    if (length(filled) > 0) {
      defaults <- default_scenario(lm_obj, raw, filled)
      scenario[names(defaults)] <- defaults
      if (verbose) {
        message(
          "Not in the scenario, so held at the mean or reference level: ",
          paste(sQuote(filled), collapse = ", "),
          "."
        )
      }
    }
  }

  missing_vars <- setdiff(predictors, names(scenario))
  if (length(missing_vars) > 0) {
    stop(
      "No value could be determined for the model variables ",
      paste(sQuote(missing_vars), collapse = ", "),
      ". Add them to the scenario.",
      call. = FALSE
    )
  }

  n_scen <- max(lengths(scenario))
  scen <- lapply(scenario[predictors], function(x) {
    if (length(x) == 1L) rep(x, n_scen) else x
  })

  # Keep factor levels aligned with the ones the model was fitted with, so
  # character input works and unseen levels are caught here rather than deep
  # inside model.frame().
  for (v in predictors) {
    levs <- lm_obj$xlevels[[v]]
    if (!is.null(levs)) {
      bad <- setdiff(as.character(scen[[v]]), levs)
      if (length(bad) > 0) {
        stop(
          "The scenario values ",
          paste(sQuote(bad), collapse = ", "),
          " for ", sQuote(v),
          " are not levels the model was fitted with: ",
          paste(sQuote(levs), collapse = ", "),
          ".",
          call. = FALSE
        )
      }
      scen[[v]] <- factor(as.character(scen[[v]]), levels = levs)
    }
  }

  scen <- as.data.frame(scen, stringsAsFactors = FALSE)

  check_log_domain(Terms, scen)

  scen
}

check_log_domain <- function(Terms, scen) {
  logged <- unique(vars_inside_log(attr(Terms, "variables")))
  logged <- intersect(logged, names(scen))

  for (v in logged) {
    x <- scen[[v]]
    if (is.numeric(x) && any(x <= 0, na.rm = TRUE)) {
      stop(
        "The variable ", sQuote(v), " enters the model through log(), but the ",
        "scenario contains values that are not positive: ",
        paste(unique(x[x <= 0]), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

# Model matrix for a data frame of scenario rows, using the terms, the stored
# predvars (so poly() reuses the fitted basis) and the fitted contrasts.
scenario_model_matrix <- function(lm_obj, scen) {
  Terms <- stats::delete.response(stats::terms(lm_obj))
  m <- stats::model.frame(Terms,
                          scen,
                          na.action = stats::na.pass,
                          xlev = lm_obj$xlevels)
  stats::model.matrix(Terms, m, contrasts.arg = lm_obj$contrasts)
}

# Short labels for the columns of the result matrices, e.g. "speed = 5".
scenario_labels <- function(scen) {
  if (is.null(scen) || nrow(scen) == 0L) {
    return(character())
  }

  # Only the variables that actually vary carry information.
  varying <- vapply(scen, function(x) length(unique(x)) > 1L, logical(1))
  if (!any(varying)) {
    varying <- rep(TRUE, ncol(scen))
  }

  parts <- lapply(names(scen)[varying], function(v) {
    x <- scen[[v]]
    if (is.numeric(x)) {
      x <- signif(x, 4)
    }
    paste0(v, " = ", as.character(x))
  })

  do.call(function(...) paste(..., sep = ", "), parts)
}

# Labels for a multiplicative counterfactual, e.g. "speed x 1.1".
multiplier_labels <- function(frame) {
  varying <- vapply(frame, function(x) length(unique(x)) > 1L, logical(1))
  if (!any(varying)) {
    varying <- rep(TRUE, ncol(frame))
  }

  parts <- lapply(names(frame)[varying], function(v) {
    paste0(v, " x ", signif(frame[[v]], 4))
  })

  do.call(function(...) paste(..., sep = ", "), parts)
}

# Column labels when the caller supplied X directly.
matrix_labels <- function(X) {
  if (!is.null(rownames(X))) {
    return(rownames(X))
  }
  paste0("scenario_", seq_len(nrow(X)))
}

# ---------------------------------------------------------------------------
# Chunking
# ---------------------------------------------------------------------------

# How many columns of an nsim by m matrix may be materialised at once so that
# the intermediate stays inside a rough element budget.
chunk_size <- function(n_total, nsim, budget = 5e6) {
  size <- max(1L, floor(budget / max(nsim, 1)))
  min(as.integer(size), as.integer(n_total))
}

chunk_index <- function(n_total, size) {
  starts <- seq.int(1L, n_total, by = size)
  lapply(starts, function(s) seq.int(s, min(s + size - 1L, n_total)))
}

example_df <- function(n = 10, b_cons = 2.5, b_educ = 0.1, sigma = 1) {
  educ <- seq(from = 10, to = 20, length.out = n)
  e <- stats::rnorm(n, mean = 0, sd = sigma)
  inc <- exp(b_cons + b_educ*educ + e)

  return(data.frame(income = inc, educ = educ))

}

setup_pop <-
  function(n,
           ff,
           type = "num",
           coefs = 1,
           zero_centered = TRUE,
           noise_sd = 1) {
    variables <- all.vars(stats::as.formula(ff))
    variable_list <- list()

    if (length(type) == length(variables) - 1) {
      type_vec <- type
    } else {
      type_vec <- rep(type[1], length(variables) - 1)
    }

    if (zero_centered == T) {
      for (variable in variables[2:length(variables)]) {
        variable_list[[paste0(variable)]] <-
          if (type_vec[variable == variables[2:length(variables)]] == "bin") {
            stats::rbinom(n, size = 1, prob = 0.5)
          } else {
            stats::rnorm(n, 0, 1)
          }
      }
    } else {
      for (variable in variables[2:length(variables)]) {
        variable_list[[paste0(variable)]] <-
          if (type_vec[variable == variables[2:length(variables)]] == "bin") {
            stats::rbinom(n, size = 1, prob = 0.5)
          } else {
            stats::rnorm(n, 7, 1.5)
          }
      }
    }
    tmp_df <- do.call(data.frame, variable_list)
    tmp_ff <- stats::as.formula(sub(".*~", "~", ff))
    mf <- stats::model.frame(tmp_ff, tmp_df)
    mm <- stats::model.matrix(tmp_ff, data = mf)
    # If the number of provided coefficients is not the same number as the number
    # of actual coefficients we take the first value of the vector and set all
    # coefficients to that value.
    if (length(coefs) == ncol(mm)) {
      coef_vec <- coefs
    } else {
      coef_vec <- rep(coefs[1], ncol(mm))
    }
    # Create the dependent variable based on the model matrix and coefficients.
    # Add some random noise.
    variable_list[[paste0(variables[1])]] <-
      mm %*% coef_vec + stats::rnorm(n, 0, noise_sd)
    df <- do.call(data.frame, variable_list)
    return(df)
  }
