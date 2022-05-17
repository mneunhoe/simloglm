rinvgamma <- function (n, shape, rate = 1, scale = 1/rate)
{
  if (missing(rate) && !missing(scale))
    rate <- 1/scale
  1/stats::rgamma(n, shape, rate)
}

lm_to_obj <- function(lm_obj){
  # extract model parameters
  beta_hat <- stats::coef(lm_obj)
  varcov_hat <- stats::vcov(lm_obj)
  sigma_hat <- summary(lm_obj)$sigma
  n <- length(stats::residuals(lm_obj))
  k <- length(beta_hat)

  return(list(beta_hat = beta_hat,
              varcov_hat = varcov_hat,
              sigma_hat = sigma_hat,
              n = n,
              k = k))
}


sim_param <- function(nsim,
                      beta_hat,
                      varcov_hat,
                      sigma_hat,
                      n,
                      k){
  beta_sim <- MASS::mvrnorm(nsim, beta_hat, varcov_hat)

  sigma2_sim <- rinvgamma(nsim, shape=(n - k)/2, scale = 2/(sigma_hat^2*(n - k)))

  return(list(betas = beta_sim, sigma = sigma2_sim))
}

sim_logy <- function(nsim, E_log_Y, sigma, exponentiate = TRUE){
  logY_sim <- stats::rnorm(nsim,
                    E_log_Y,
                    sigma)
  if (exponentiate == TRUE){return(exp(logY_sim))}
  if (exponentiate == FALSE){return(logY_sim)}
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
