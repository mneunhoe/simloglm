lm_to_obj <- function(lm_obj){
  # extract model parameters
  beta_hat <- coef(lm_obj)
  varcov_hat <- vcov(lm_obj)
  sigma_hat <- summary(lm_obj)$sigma
  n <- length(residuals(lm_obj))
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
  #sigma2_sim <- sigma_hat^2 * (n-k) / rchisq(nsim_est, n-k)
  sigma2_sim <- rgamma(nsim, shape = (n-k)/2, rate = (n-k)/(2*sigma_hat^2))
  return(list(betas = beta_sim, sigma = sigma2_sim))
}

sim_logy <- function(nsim, E_log_Y, sigma, exponentiate = TRUE){
  logY_sim <- rnorm(nsim,
                    E_log_Y,
                    sigma)
  if (exponentiate == TRUE){return(exp(logY_sim))}
  if (exponentiate == FALSE){return(logY_sim)}
}
