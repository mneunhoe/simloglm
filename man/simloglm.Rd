% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simloglm.R
\name{simloglm}
\alias{simloglm}
\title{Simulate from a linear regression model with a logged dependent variable}
\usage{
simloglm(
  input_obj,
  nsim_est = 1000,
  nsim_fund = 1000,
  scenario = NULL,
  X = NULL,
  observed_value_approach = FALSE,
  logged_dv = TRUE,
  predicted_values = FALSE,
  fast = TRUE
)
}
\arguments{
\item{input_obj}{Either a list of class "lm" (the output from a call to lm) or a user provided list with the following entries beta_hat (the estimated regression coefficients),
unscaled_vcov (the unscaled variance covariance matrix), sigma_hat (the estimated residual standard error), n (the number of observations) and k (the number of regression coefficients).
The list can be provided for more flexibility. Most users will call simulate directly on the output from a call to lm.}

\item{nsim_est}{Number of simulations to simulate estimation uncertainty (defaults to 1000).}

\item{nsim_fund}{Number of simulations to simulate fundamental uncertainty (defaults to 1000).}

\item{scenario}{Named list with values (scalar or vector, the vectors need to be of the same length) for each independent variable (names must match the names of the variables in the regression model) of scenarios for which predictions of the regression model should be calculated. (Default is at the mean for all variables only works when using a lm object.)}

\item{X}{Sometimes it is easier to directly pass the scenario as a model matrix (e.g. when you want to set the average of factors), this overrides any scenario specified under scenario. (Default is NULL)}

\item{observed_value_approach}{Do you want to use the observed value approach? (Default is FALSE.)}

\item{logged_dv}{Is the dependent variable in the linear model logged? (Default is TRUE.)}

\item{predicted_values}{Do you also want to output predicted values? (Default is FALSE. The resulting object can be very big.)}

\item{fast}{Do you want to speed up computation by not explicitly calculating predicted values? (Default is FALSE.)}
}
\value{
A list of class "simloglm" with two matrices (geometric_mean and arithmetic_mean) with each nsim_est rows and number of scenarios columns and a matrix of predicted values if desired.
}
\description{
Simulate from a linear regression model with a logged dependent variable
}
\examples{
df <- cars
regression <- lm(log(dist)~speed, data = df)
# Specifiying no scenario to simulate at the mean of speed.
simloglm(regression)
# Explicitily specifying a scenario.
simloglm(regression, scenario = list(speed = c(5, 10, 20)))
}
