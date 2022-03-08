#' Simulate from a linear regression model with a logged dependent variable
#'
#' @param input_obj Either a list of class "lm" (the output from a call to lm) or a user provided list with the following entries beta_hat (the estimated regression coefficients),
#' varcov_hat (the estimated variance covariance matrix), sigma_hat (the estimated residual standard error), n (the number of observations) and k (the number of regression coefficients).
#' The list can be provided for more flexibility. Most users will call simulate directly on the output from a call to lm.
#' @param nsim_est Number of simulations to simulate estimation uncertainty (defaults to 1000).
#' @param nsim_fund Number of simulations to simulate fundamental uncertainty (defaults to 1000).
#' @param scenario Named list with values (scalar or vector, the vectors need to be of the same length) for each independent variable (names must match the names of the variables in the regression model) of scenarios for which predictions of the regression model should be calculated. (Default is at the mean for all variables only works when using a lm object.)
#' @param X Sometimes it is easier to directly pass the scenario as a model matrix (e.g. when you want to set the average of factors), this overrides any scenario specified under scenario. (Default is NULL)
#' @param logged_dv Is the dependent variable in the linear model logged? (Default is TRUE.)
#' @param predicted_values Do you also want to output predicted values? (Default is FALSE. The resulting object can be very big.)
#' @return A list of class "simloglm" with two matrices (geometric_mean and arithmetic_mean) with each nsim_est rows and number of scenarios columns and a matrix of predicted values if desired.
#' @export
#'
#' @examples
#' df <- cars
#' regression <- lm(log(dist)~speed, data = df)
#' # Specifiying no scenario to simulate at the mean of speed.
#' simloglm(regression)
#' # Explicitily specifying a scenario.
#' simloglm(regression, scenario = list(speed = c(5, 10, 20)))
simloglm <- function(input_obj,
                     nsim_est = 1000,
                     nsim_fund = 1000,
                     scenario = NULL,
                     X = NULL,
                     logged_dv = TRUE,
                     predicted_values = FALSE) {
  # Handle lm objects
  if (class(input_obj) == "lm") {
    # Automatically detect logged dependent variables
    if (is.null(logged_dv)) {
      logged_dv <- grepl("log\\(", colnames(input_obj$model)[1])
    }
    # Set default scenario (to mean values)
    # if (is.null(scenario)) {
    #   scenario <-
    #     cbind(1, colMeans(input_obj$model)[2:ncol(input_obj$model)])
    #   rownames(scenario) <- NULL
    #   colnames(scenario) <-
    #     c("(Intercept)", colnames(input_obj$model)[2:ncol(input_obj$model)])
    # }
    obj <- lm_to_obj(input_obj)
  }
  # estimation uncertainty
  parameters_sim <- sim_param(
    nsim = nsim_est,
    beta_hat = obj$beta_hat,
    sigma_hat = obj$sigma_hat,
    varcov_hat = obj$varcov_hat,
    n = obj$n,
    k = obj$k
  )
  beta_sim <- parameters_sim$betas
  sigma2_sim <-  parameters_sim$sigma

  if (is.null(X)) {
    if (class(input_obj) == "lm") {
      if (is.list(scenario)) {
        scen <- do.call(data.frame, scenario)
      } else if (is.null(scenario)) {
        message("No scenario provided, all variables were set to their mean.")

        scen <- as.data.frame.list(colMeans(input_obj$model))
      }

      # Same as in predict.lm
      tt <- stats::terms(input_obj)


      Terms <- stats::delete.response(tt)
      m <- stats::model.frame(Terms,
                              scen,
                              na.action = stats::na.pass,
                              xlev = input_obj$xlevels)

      X <-
        stats::model.matrix(Terms, m, contrasts.arg = input_obj$contrasts)
    } else if (is.list(scenario) & class(input_obj) != "lm") {
      stop(
        "Passing a list for the scenario only works with a lm object. \nPlease provide a valid model matrix."
      )
    } else if (!is.null(scenario)) {
      if ("matrix" %in% class (scenario) &
          ncol(scenario) == length(obj$beta_hat)) {
        message("Check that the columns of the scenario are in the same order as the coefficients.")
        X <- scenario
      } else if ("numeric" == class(scenario)) {
        message("Check that the entries of the scenario are in the same order as the coefficients.")
        X <- matrix(scenario, nrow = 1)
      }
    } else {
      stop("No valid scenario specified.")
    }
  }


  # Calculate Expected Values of log(Y)
  Xbeta_sim <- parameters_sim$betas %*% t(X)


  # Add fundamental uncertainty

  res <- array(NA, c(nsim_est, nrow(X), 2))

  if (predicted_values) {
    pv <- vector("list", length = nrow(X))
  }

  for (s in 1:nrow(X)) {
    for (i in 1:nsim_est) {
      y_sim <- sim_logy(
        nsim = nsim_fund,
        E_log_Y = Xbeta_sim[i, s],
        sigma = sqrt(parameters_sim$sigma[i]),
        exponentiate = TRUE
      )


      if (predicted_values) {
        pv[[s]] <- cbind(pv[[s]], y_sim)
      }



      res[i, s, 1] <- exp(mean(log(y_sim)))


      res[i, s, 2] <- mean(y_sim)

    }

    if (predicted_values) {
      colnames(pv[[s]]) <- NULL
    }
  }


  if (!predicted_values) {
    pv <- list()
  }


  result_object <- list(
    geometric_mean = res[, , 1],
    arithmetic_mean = res[, , 2],
    predicted_values = pv
  )

  class(result_object) <- "simloglm"

  return(result_object)
}
