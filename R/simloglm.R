#' Simulate from a linear regression model with a logged dependent variable
#'
#' @param input_obj Either an object of class "lm" (the output from a call to
#'   lm) or a user provided list with the entries beta_hat (the estimated
#'   regression coefficients), unscaled_vcov (the unscaled variance covariance
#'   matrix), sigma_hat (the estimated residual standard error), n (the number of
#'   observations) and k (the number of regression coefficients). The list can be
#'   provided for more flexibility. Most users will call simloglm directly on the
#'   output from a call to lm.
#' @param nsim_est Number of simulations to simulate estimation uncertainty
#'   (defaults to 1000).
#' @param nsim_fund Number of simulations to simulate fundamental uncertainty
#'   (defaults to 1000). Only used when `fast = FALSE`.
#' @param scenario Named list with values (scalar or vector; vectors must all
#'   have the same length, scalars are recycled) for the independent variables.
#'   The names must match the *variables* of the regression model, and the values
#'   are given on the **original scale**: for `log(dist) ~ log(speed)` you supply
#'   `speed`, not `log(speed)`, and the `log()` is applied for you. Variables you
#'   leave out are held at their mean (numeric) or reference level (factor).
#'   Defaults to `NULL`, which holds every variable at its mean or reference
#'   level.
#' @param X Sometimes it is easier to pass the scenario directly as a model
#'   matrix (e.g. when you want to set factors to their observed proportions).
#'   This overrides `scenario`. In contrast to `scenario`, the columns of `X`
#'   must already be **transformed**: they are used as the rows of the model
#'   matrix exactly as supplied, in the order of the coefficients. (Default is
#'   NULL.)
#' @param multiplier Named list of multiplicative shifts, e.g.
#'   `list(income = c(1, 1.1))` for the observed sample and a ten percent
#'   increase. Only available together with `observed_value_approach = TRUE`,
#'   and mutually exclusive with `scenario`. A proportional shift is the natural
#'   counterfactual under an elasticity, where setting everybody to the same
#'   value is not. (Default is NULL.)
#' @param observed_value_approach Do you want to use the observed value
#'   approach? Instead of predicting for one synthetic average case, the
#'   counterfactual is applied to every observation in the estimation sample and
#'   the predictions are averaged. (Default is FALSE.)
#' @param logged_dv Is the dependent variable in the linear model logged?
#'   Defaults to `TRUE`, the premise of this package. Set it to `FALSE` to
#'   simulate on the scale of the dependent variable, without the exponentiation
#'   and without the sigma^2 / 2 correction. Pass `NULL` to have it guessed from
#'   the model formula -- note that this is only a guess, because a dependent
#'   variable that was logged before the model was fitted (`logdist ~ .`) is
#'   indistinguishable from one that was not.
#' @param predicted_values Do you also want to output predicted values? (Default
#'   is FALSE. The resulting object can be very big.) Requires `fast = FALSE`.
#' @param fast Do you want to speed up computation by not explicitly simulating
#'   predicted values? The fast path evaluates the same quantities in closed form
#'   and should be preferred for point estimates. (Default is TRUE.)
#' @param verbose Print progress bars and informational messages? (Default is
#'   TRUE.)
#' @return A list of class "simloglm" with the entries `median` and `mean`: two
#'   matrices with `nsim_est` rows and one column per scenario, holding the
#'   simulated conditional geometric mean (equivalently, the median) and the
#'   conditional arithmetic mean of the dependent variable. `median_point_estimate`
#'   and `mean_point_estimate` hold the corresponding quantities evaluated at the
#'   coefficient estimates, and `predicted_values` a list of predicted value
#'   matrices if these were requested.
#' @export
#'
#' @examples
#' df <- cars
#' regression <- lm(log(dist) ~ speed, data = df)
#'
#' # Specifying no scenario simulates at the mean of speed.
#' at_the_mean <- simloglm(regression, verbose = FALSE)
#' get_summary(at_the_mean)
#'
#' # Explicitly specifying a scenario.
#' res <- simloglm(regression, scenario = list(speed = c(5, 10, 20)),
#'                 verbose = FALSE)
#' get_summary(res)
#'
#' # Scenario values are given on the original scale, also in a log-log model.
#' loglog <- lm(log(dist) ~ log(speed), data = df)
#' res_loglog <- simloglm(loglog, scenario = list(speed = c(10, 11)),
#'                        verbose = FALSE)
#' get_ratio(res_loglog)

simloglm <- function(input_obj,
                     nsim_est = 1000,
                     nsim_fund = 1000,
                     scenario = NULL,
                     X = NULL,
                     multiplier = NULL,
                     observed_value_approach = FALSE,
                     logged_dv = TRUE,
                     predicted_values = FALSE,
                     fast = TRUE,
                     verbose = TRUE) {
  is_lm <- inherits(input_obj, "lm")

  # The formula can only ever be a hint: a dependent variable that was logged
  # before the fit ("logdist ~ .") looks unlogged and vice versa. So the default
  # is TRUE, the premise of this package, and detection has to be asked for by
  # passing logged_dv = NULL.
  looks_logged <- if (is_lm) {
    grepl("^log(2|10)?\\(", deparse(stats::formula(input_obj)[[2L]]))
  } else {
    NA
  }

  if (is.null(logged_dv)) {
    logged_dv <- isTRUE(looks_logged)
    if (verbose && is_lm) {
      message(
        "The dependent variable ",
        sQuote(deparse(stats::formula(input_obj)[[2L]])),
        if (logged_dv) " looks logged" else " does not look logged",
        ", so logged_dv was set to ", logged_dv, "."
      )
    }
  } else if (isTRUE(logged_dv) && isFALSE(looks_logged) && verbose) {
    message(
      "The dependent variable ",
      sQuote(deparse(stats::formula(input_obj)[[2L]])),
      " does not look logged, but logged_dv is TRUE, so results are ",
      "exponentiated and the sigma^2 / 2 correction is applied. If it is ",
      "already on the original scale, pass logged_dv = FALSE."
    )
  }

  if (is_lm) {
    obj <- lm_to_obj(input_obj)
  } else {
    obj <- check_obj(input_obj)
  }

  if (!is.null(multiplier) && !observed_value_approach) {
    stop(
      "multiplier is only available with observed_value_approach = TRUE. ",
      "Without it every observation is replaced by the same scenario, so there ",
      "is nothing to shift proportionally.",
      call. = FALSE
    )
  }

  # estimation uncertainty
  parameters_sim <- sim_param(
    nsim = nsim_est,
    beta_hat = obj$beta_hat,
    sigma_hat = obj$sigma_hat,
    unscaled_vcov = obj$unscaled_vcov,
    n = obj$n,
    k = obj$k
  )
  beta_sim <- parameters_sim$betas
  sigma2_sim <- parameters_sim$sigma

  scen <- NULL

  if (observed_value_approach) {
    if (!is_lm) {
      stop("The observed value approach currently only works with lm objects.",
           call. = FALSE)
    }
    if (!fast) {
      stop("The observed value approach only works with the fast option.",
           call. = FALSE)
    }
    if (predicted_values && verbose) {
      message("Predicted values cannot be calculated when using the fast option.")
    }

    ov <- observed_value_qoi(
      lm_obj = input_obj,
      obj = obj,
      beta_sim = beta_sim,
      scenario = scenario,
      multiplier = multiplier,
      logged_dv = logged_dv,
      verbose = verbose
    )

    labels <- ov$labels
    scen <- ov$scenario

    if (logged_dv) {
      # rowMeans(exp(Xb + s2 / 2)) = exp(s2 / 2) * rowMeans(exp(Xb)).
      sim_median <- ov$value
      sim_mean <- ov$value * exp(sigma2_sim / 2)
      median_pe <- ov$value_pe
      mean_pe <- ov$value_pe * exp(obj$sigma_hat^2 / 2)
    } else {
      sim_median <- ov$value
      sim_mean <- ov$value
      median_pe <- ov$value_pe
      mean_pe <- ov$value_pe
    }

    pv <- list()

  } else {
    if (is.null(X)) {
      if (is_lm) {
        scen <- build_scenario_frame(input_obj, scenario, verbose = verbose)
        X <- scenario_model_matrix(input_obj, scen)
        labels <- scenario_labels(scen)
      } else if (is.list(scenario)) {
        stop(
          "Passing a list for the scenario only works with a lm object. \nPlease provide a valid model matrix.",
          call. = FALSE
        )
      } else if (!is.null(scenario)) {
        if (is.matrix(scenario) && ncol(scenario) == obj$k) {
          if (verbose) {
            message(
              "Check that the columns of the scenario are in the same order as the coefficients."
            )
          }
          X <- scenario
        } else if (is.numeric(scenario) && length(scenario) == obj$k) {
          if (verbose) {
            message(
              "Check that the entries of the scenario are in the same order as the coefficients."
            )
          }
          X <- matrix(scenario, nrow = 1)
        } else {
          stop(
            "The scenario must be a matrix with ", obj$k,
            " columns or a numeric vector of length ", obj$k, ".",
            call. = FALSE
          )
        }
        labels <- matrix_labels(X)
      } else {
        stop("No valid scenario specified.", call. = FALSE)
      }
    } else {
      X <- as.matrix(X)
      labels <- matrix_labels(X)
    }

    if (ncol(X) != obj$k) {
      stop(
        "The model matrix has ", ncol(X), " columns but the model has ", obj$k,
        " coefficients.",
        call. = FALSE
      )
    }

    n_scen <- nrow(X)

    # Expected value of log(Y) (or of Y, when the dv is not logged).
    Xbeta_sim <- beta_sim %*% t(X)
    Xbeta_hat <- as.vector(X %*% obj$beta_hat)

    if (fast) {
      if (predicted_values && verbose) {
        message("Predicted values cannot be calculated when using the fast option.")
      }
      pv <- list()

      if (logged_dv) {
        # exp(Xb + s2 / 2) = exp(Xb) * exp(s2 / 2): exponentiate the big matrix
        # once and scale by a length-nsim_est vector, recycling down columns.
        sim_median <- exp(Xbeta_sim)
        sim_mean <- sim_median * exp(sigma2_sim / 2)
      } else {
        sim_median <- Xbeta_sim
        sim_mean <- Xbeta_sim
      }

    } else {
      sim_median <- matrix(NA_real_, nsim_est, n_scen)
      sim_mean <- matrix(NA_real_, nsim_est, n_scen)

      pv <- if (predicted_values) {
        replicate(n_scen, matrix(NA_real_, nsim_fund, nsim_est), simplify = FALSE)
      } else {
        list()
      }

      sd_sim <- sqrt(sigma2_sim)
      size <- chunk_size(nsim_est, nsim_fund)
      chunks <- chunk_index(nsim_est, size)

      if (verbose) {
        cli::cli_progress_bar("Running the simulation",
                              total = n_scen * length(chunks),
                              .envir = environment())
      }

      for (s in seq_len(n_scen)) {
        for (idx in chunks) {
          # One rnorm() call per chunk instead of one per draw.
          logy <- matrix(stats::rnorm(length(idx) * nsim_fund),
                         nrow = length(idx), ncol = nsim_fund)
          logy <- logy * sd_sim[idx] + Xbeta_sim[idx, s]

          if (logged_dv) {
            # Draw on the log scale, average there for the geometric mean, and
            # exponentiate once.
            sim_median[idx, s] <- exp(rowMeans(logy))
            sim_mean[idx, s] <- rowMeans(exp(logy))
            if (predicted_values) {
              pv[[s]][, idx] <- t(exp(logy))
            }
          } else {
            m <- rowMeans(logy)
            sim_median[idx, s] <- m
            sim_mean[idx, s] <- m
            if (predicted_values) {
              pv[[s]][, idx] <- t(logy)
            }
          }

          if (verbose) {
            cli::cli_progress_update(.envir = environment())
          }
        }
      }

      if (verbose) {
        cli::cli_progress_done(.envir = environment())
      }
    }

    if (logged_dv) {
      median_pe <- exp(Xbeta_hat)
      mean_pe <- median_pe * exp(obj$sigma_hat^2 / 2)
    } else {
      median_pe <- Xbeta_hat
      mean_pe <- Xbeta_hat
    }
  }

  colnames(sim_median) <- labels
  colnames(sim_mean) <- labels
  names(median_pe) <- labels
  names(mean_pe) <- labels
  if (length(pv) > 0) {
    names(pv) <- labels
  }

  result_object <- list(
    median_point_estimate = median_pe,
    median = sim_median,
    mean_point_estimate = mean_pe,
    mean = sim_mean,
    predicted_values = pv,
    scenario = scen,
    logged_dv = logged_dv,
    observed_value_approach = observed_value_approach
  )

  class(result_object) <- "simloglm"

  return(result_object)
}


# ---------------------------------------------------------------------------
# Observed value approach
# ---------------------------------------------------------------------------

# Returns, for every scenario, the average over the estimation sample of the
# conditional geometric mean exp(X_i b) -- one column per scenario, one row per
# simulation draw, plus the same quantity at the coefficient estimates. When the
# dependent variable is not logged the average of X_i b is returned instead.
observed_value_qoi <- function(lm_obj,
                               obj,
                               beta_sim,
                               scenario,
                               multiplier,
                               logged_dv = TRUE,
                               verbose = TRUE) {
  if (!is.null(scenario) && !is.null(multiplier)) {
    stop("Supply either a scenario or a multiplier, not both.", call. = FALSE)
  }

  raw <- model_data(lm_obj)
  predictors <- all.vars(stats::formula(stats::delete.response(stats::terms(lm_obj))))

  spec <- ov_spec(lm_obj, raw, predictors, scenario, multiplier, verbose)

  # Stack the point estimate onto the draws so the expensive part is computed
  # once for both.
  B <- rbind(beta_sim, obj$beta_hat)
  nsim <- nrow(beta_sim)

  value <- if (spec$shortcut) {
    ov_shortcut(lm_obj, raw, B, spec, logged_dv)
  } else {
    ov_rebuild(lm_obj, raw, B, spec, logged_dv, verbose)
  }

  list(
    value = value[seq_len(nsim), , drop = FALSE],
    value_pe = value[nsim + 1L, ],
    labels = spec$labels,
    scenario = spec$frame
  )
}

# Work out what each scenario does to the data, and whether the shortcut below
# applies: it does exactly when every model term touched by a scenario variable
# involves only scenario variables, so that setting them to a constant makes the
# affected columns of X constant across observations.
ov_spec <- function(lm_obj, raw, predictors, scenario, multiplier, verbose) {
  if (is.null(scenario) && is.null(multiplier)) {
    if (verbose) {
      message("No scenario provided, using the estimation sample as observed.")
    }
    return(list(
      shortcut = FALSE,
      mods = list(list()),
      labels = "observed",
      frame = NULL
    ))
  }

  if (!is.null(multiplier)) {
    spec <- ov_check_named(multiplier, predictors, "multiplier")
    if (!all(vapply(spec, is.numeric, logical(1)))) {
      stop("Every multiplier must be numeric.", call. = FALSE)
    }
    not_numeric <- names(spec)[!vapply(names(spec),
                                       function(v) is.numeric(raw[[v]]),
                                       logical(1))]
    if (length(not_numeric) > 0) {
      stop(
        "A multiplier only makes sense for a numeric variable, but ",
        paste(sQuote(not_numeric), collapse = ", "),
        " is not one. Use a scenario instead.",
        call. = FALSE
      )
    }
    logged <- unique(vars_inside_log(attr(stats::terms(lm_obj), "variables")))
    for (v in intersect(names(spec), logged)) {
      if (any(spec[[v]] <= 0)) {
        stop(
          "The variable ", sQuote(v), " enters the model through log(), so its ",
          "multiplier must be positive.",
          call. = FALSE
        )
      }
    }
    frame <- as.data.frame(spec, stringsAsFactors = FALSE)
    mods <- lapply(seq_len(nrow(frame)), function(j) {
      list(mult = as.list(frame[j, , drop = FALSE]))
    })
    labels <- multiplier_labels(frame)
    # A multiplier rescales each observation differently, so the affected
    # columns of X are not constant and the shortcut does not apply.
    return(list(shortcut = FALSE, mods = mods, labels = labels, frame = frame))
  }

  spec <- ov_check_named(scenario, predictors, "scenario")
  frame <- as.data.frame(spec, stringsAsFactors = FALSE)

  for (v in names(frame)) {
    levs <- lm_obj$xlevels[[v]]
    if (!is.null(levs)) {
      bad <- setdiff(as.character(frame[[v]]), levs)
      if (length(bad) > 0) {
        stop(
          "The scenario values ", paste(sQuote(bad), collapse = ", "),
          " for ", sQuote(v), " are not levels the model was fitted with.",
          call. = FALSE
        )
      }
      frame[[v]] <- factor(as.character(frame[[v]]), levels = levs)
    }
  }

  check_log_domain(stats::delete.response(stats::terms(lm_obj)), frame)

  mods <- lapply(seq_len(nrow(frame)), function(j) {
    list(set = as.list(frame[j, , drop = FALSE]))
  })

  vars <- names(frame)
  term_vars <- lapply(attr(stats::terms(lm_obj), "term.labels"),
                      function(l) all.vars(stats::as.formula(paste("~", l))))
  touched <- vapply(term_vars, function(tv) any(tv %in% vars), logical(1))
  closed <- all(vapply(term_vars[touched], function(tv) all(tv %in% vars), logical(1)))

  list(
    shortcut = closed,
    mods = mods,
    labels = scenario_labels(frame),
    frame = frame,
    vars = vars,
    touched = which(touched)
  )
}

ov_check_named <- function(x, predictors, what) {
  if (!is.list(x) || is.null(names(x)) || any(names(x) == "")) {
    stop(what, " must be a named list.", call. = FALSE)
  }

  unknown <- setdiff(names(x), predictors)
  if (length(unknown) > 0) {
    stop(
      "The ", what, " names ", paste(sQuote(unknown), collapse = ", "),
      " are not variables in the model. Available variables: ",
      paste(sQuote(predictors), collapse = ", "), ".",
      call. = FALSE
    )
  }

  lens <- lengths(x)
  n_scen <- max(lens)
  if (any(lens != 1L & lens != n_scen)) {
    stop(
      "All ", what, " entries must have length 1 or the same length. Got: ",
      paste(paste0(names(x), " (", lens, ")"), collapse = ", "), ".",
      call. = FALSE
    )
  }

  lapply(x, function(v) if (length(v) == 1L) rep(v, n_scen) else v)
}

# The affected columns of X are constant across observations, so
#   X_i b = (unaffected part)_i + x_s . b_affected
# and the average of exp(X_i b) factors into a term that is computed once for
# all scenarios and a cheap per-scenario factor. The large matrix product and
# exp() happen exactly once, no matter how many scenario values are requested.
ov_shortcut <- function(lm_obj, raw, B, spec, logged_dv) {
  X <- scenario_model_matrix(lm_obj, raw)
  cols <- which(attr(X, "assign") %in% spec$touched)
  rest <- setdiff(seq_len(ncol(X)), cols)

  # One row per scenario of the affected columns of the model matrix. Built
  # through the stored terms, so poly() reuses the fitted basis.
  template <- raw[rep(1L, length(spec$mods)), , drop = FALSE]
  for (j in seq_along(spec$mods)) {
    set <- spec$mods[[j]]$set
    for (v in names(set)) {
      if (is.factor(template[[v]])) {
        template[[v]][j] <- as.character(set[[v]])
      } else {
        template[[v]][j] <- set[[v]]
      }
    }
  }
  Xs <- scenario_model_matrix(lm_obj, template)[, cols, drop = FALSE]

  # The unaffected part, accumulated in chunks of observations so that peak
  # memory does not grow with n.
  n <- nrow(X)
  acc <- numeric(nrow(B))
  Brest <- B[, rest, drop = FALSE]
  Xrest <- X[, rest, drop = FALSE]
  for (idx in chunk_index(n, chunk_size(n, nrow(B)))) {
    Xb <- Brest %*% t(Xrest[idx, , drop = FALSE])
    acc <- acc + rowSums(if (logged_dv) exp(Xb) else Xb)
  }
  base <- acc / n

  Bs <- B[, cols, drop = FALSE] %*% t(Xs)
  if (logged_dv) exp(Bs) * base else Bs + base
}

# General fallback: rebuild the model matrix per scenario, but recover the raw
# data and the terms only once and chunk over observations.
ov_rebuild <- function(lm_obj, raw, B, spec, logged_dv, verbose) {
  n <- nrow(raw)
  n_scen <- length(spec$mods)
  out <- matrix(NA_real_, nrow(B), n_scen)
  size <- chunk_size(n, nrow(B))

  show_progress <- verbose && n_scen > 1L
  if (show_progress) {
    cli::cli_progress_bar("Running the simulation", total = n_scen)
  }

  for (j in seq_len(n_scen)) {
    d <- apply_mod(raw, spec$mods[[j]])
    X <- scenario_model_matrix(lm_obj, d)

    acc <- numeric(nrow(B))
    for (idx in chunk_index(n, size)) {
      Xb <- B %*% t(X[idx, , drop = FALSE])
      acc <- acc + rowSums(if (logged_dv) exp(Xb) else Xb)
    }
    out[, j] <- acc / n

    if (show_progress) {
      cli::cli_progress_update()
    }
  }

  if (show_progress) {
    cli::cli_progress_done()
  }

  out
}

apply_mod <- function(raw, mod) {
  d <- raw
  for (v in names(mod$set)) {
    d[[v]] <- mod$set[[v]]
  }
  for (v in names(mod$mult)) {
    d[[v]] <- d[[v]] * mod$mult[[v]]
  }
  d
}
