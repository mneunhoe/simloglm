# A deliberately naive observed-value implementation: rebuild the whole model
# matrix for every scenario value and average over observations. Slow and
# memory hungry, but obviously correct, so the optimised paths in simloglm()
# can be checked against it.
observed_value_reference <- function(lm_obj, scenario, nsim_est = 50) {
  obj <- lm_to_obj(lm_obj)
  pars <- sim_param(
    nsim = nsim_est,
    beta_hat = obj$beta_hat,
    unscaled_vcov = obj$unscaled_vcov,
    sigma_hat = obj$sigma_hat,
    n = obj$n,
    k = obj$k
  )
  beta_sim <- pars$betas
  sigma2_sim <- pars$sigma

  Terms <- stats::delete.response(stats::terms(lm_obj))
  raw <- model_data(lm_obj)

  n_scen <- max(lengths(scenario))
  med <- matrix(NA_real_, nsim_est, n_scen)
  avg <- matrix(NA_real_, nsim_est, n_scen)

  for (j in seq_len(n_scen)) {
    d <- raw
    for (v in names(scenario)) {
      value <- scenario[[v]]
      d[[v]] <- if (length(value) == 1L) value else value[j]
    }
    m <- stats::model.frame(Terms, d, na.action = stats::na.pass,
                            xlev = lm_obj$xlevels)
    X <- stats::model.matrix(Terms, m, contrasts.arg = lm_obj$contrasts)
    Xbeta <- beta_sim %*% t(X)
    med[, j] <- rowMeans(exp(Xbeta))
    avg[, j] <- rowMeans(exp(Xbeta + sigma2_sim / 2))
  }

  list(median = med, mean = avg)
}
