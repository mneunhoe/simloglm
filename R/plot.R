#' Plot function for a simloglm object
#'
#' @param input_obj Either a list of class "lm" (the output from a call to lm) or a user provided list with the following entries beta_hat (the estimated regression coefficients),
#' varcov_hat (the estimated variance covariance matrix), sigma_hat (the estimated residual standard error), n (the number of observations) and k (the number of regression coefficients).
#' The list can be provided for more flexibility. Most users will call simulate directly on the output from a call to lm.
#' @param which_qoi Which mean should be plotted? Median or mean, defaults to "median".
#' @param alpha Set the significance level. Default is 0.05.
#' @param twosided Do you want two sided confidence intervals? Defaults to TRUE.
#' @return A plot of the simloglm object.
#' @export
#'
#' @examples
#' df <- cars
#' regression <- lm(log(dist)~speed, data = df)
#' # Specifiying no scenario to simulate at the mean of speed.
#' plot(simloglm(regression))
#' # Explicitily specifying a scenario.
#' plot(simloglm(regression, scenario = list(speed = c(5, 10, 20))))

plot.simloglm <- function(input_obj, which_qoi = "median", alpha = 0.05, twosided = TRUE) {

    plot(input_obj[[which_qoi]])


}
