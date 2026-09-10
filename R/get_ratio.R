#' Get the ratio between two scenarios from a simloglm object
#'
#' The ratio of two predicted values is the natural quantity of interest in a
#' log-log model, where it is the multiplicative effect implied by the
#' elasticity: raising `x` by ten percent multiplies the predicted `y` by
#' `1.1^beta`.
#'
#' @section The sigma^2 / 2 correction cancels:
#' Unlike a level prediction or a first difference, a ratio does **not** depend
#' on the correction that this package exists to apply:
#' \deqn{\frac{\exp(X_2\beta + \sigma^2/2)}{\exp(X_1\beta + \sigma^2/2)} =
#'   \exp((X_2 - X_1)\beta).}
#' In the fast path the cancellation happens draw by draw, because row *i* of
#' both scenarios uses the same `sigma2_sim[i]`. `which_qoi = "median"` and
#' `which_qoi = "mean"` therefore return numerically identical distributions,
#' and that is the correct answer rather than a bug. The correction has bite for
#' level predictions and for [get_first_difference()], not for ratios.
#'
#' @param input_obj An object of class simloglm (the output of calling
#'   simloglm), with at least two scenarios.
#' @param which_qoi Are you interested in the conditional geometric mean
#'   (`"median"`) or the conditional arithmetic mean (`"mean"`)? Default is
#'   `"median"`. See the section above: for a ratio the two agree.
#' @param which_scenarios If you pass a simloglm object with more than two
#'   scenarios please specify which scenarios should be used. Default is
#'   `c(1, 2)`, and the ratio is the second scenario over the first.
#' @param alpha The significance level for the resulting confidence interval.
#'   Default is 0.05.
#' @return A list of class "ratio_simloglm" with the ratio and its simulation
#'   quantiles.
#' @export
#'
#' @examples
#' df <- cars
#' regression <- lm(log(dist) ~ log(speed), data = df)
#' # A ten percent increase in speed, from the elasticity.
#' simulation_results <- simloglm(regression, scenario = list(speed = c(10, 11)),
#'                                verbose = FALSE)
#' ratio_summary <- get_ratio(simulation_results)
#' ratio_summary

get_ratio <-
  function(input_obj,
           which_qoi = "median",
           which_scenarios = c(1, 2),
           alpha = 0.05) {
    qoi <- check_simloglm(input_obj, which_qoi, min_scenarios = 2L)
    pair <- pick_scenarios(qoi, which_scenarios)

    point_estimate <-
      input_obj[[paste0(which_qoi, "_point_estimate")]][which_scenarios]

    ratio <- unname(point_estimate[2] / point_estimate[1])

    ratio_dist <- pair[, 2] / pair[, 1]
    # A one column matrix, so that both q[1] and q[1, ] keep working and the
    # shape matches get_summary()'s one column per scenario.
    quantiles <-
      matrix(stats::quantile(ratio_dist, c(alpha / 2, 1 - alpha / 2)),
             ncol = 1,
             dimnames = list(paste0(100 * c(alpha / 2, 1 - alpha / 2), "%"), NULL))

    result_object <- list(ratio = ratio,
                          quantiles = quantiles,
                          scenarios = colnames(pair))

    class(result_object) <- "ratio_simloglm"

    return(result_object)
  }
