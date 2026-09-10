#' Get the first difference from a simloglm object
#'
#' The first difference between two scenarios, on the scale of the dependent
#' variable. In contrast to [get_ratio()], a first difference is
#' baseline-dependent: the same proportional change produces a different
#' absolute difference depending on where you start, and the sigma^2 / 2
#' correction does **not** cancel. The baseline is returned alongside the
#' difference and should be reported with it.
#'
#' @param input_obj An object of class simloglm (the output of calling
#'   simloglm), with at least two scenarios.
#' @param which_qoi Are you interested in the conditional geometric mean
#'   (`"median"`) or the conditional arithmetic mean (`"mean"`)? Default is
#'   `"median"`.
#' @param which_scenarios If you pass a simloglm object with more than two
#'   scenarios please specify which scenarios should be used. Default is
#'   `c(1, 2)`, and the difference is the second scenario minus the first.
#' @param alpha The significance level for the resulting confidence interval.
#'   Default is 0.05.
#' @return A list of class "first_difference_simloglm" with the first
#'   difference, the baseline it is measured from, and simulation quantiles.
#' @export
#'
#' @examples
#' df <- cars
#' regression <- lm(log(dist) ~ speed, data = df)
#' # Explicitly specifying two scenarios.
#' simulation_results <- simloglm(regression, scenario = list(speed = c(5, 20)),
#'                                verbose = FALSE)
#' first_difference_summary <- get_first_difference(simulation_results)
#' first_difference_summary

get_first_difference <-
  function(input_obj,
           which_qoi = "median",
           which_scenarios = c(1, 2),
           alpha = 0.05) {
    qoi <- check_simloglm(input_obj, which_qoi, min_scenarios = 2L)
    pair <- pick_scenarios(qoi, which_scenarios)

    point_estimate <-
      input_obj[[paste0(which_qoi, "_point_estimate")]][which_scenarios]

    fd <- unname(point_estimate[2] - point_estimate[1])

    fd_dist <- pair[, 2] - pair[, 1]
    # A one column matrix, so that both q[1] and q[1, ] keep working and the
    # shape matches get_summary()'s one column per scenario.
    quantiles <-
      matrix(stats::quantile(fd_dist, c(alpha / 2, 1 - alpha / 2)),
             ncol = 1,
             dimnames = list(paste0(100 * c(alpha / 2, 1 - alpha / 2), "%"), NULL))

    result_object <- list(fd = fd,
                          baseline = unname(point_estimate[1]),
                          quantiles = quantiles,
                          scenarios = colnames(pair))

    class(result_object) <- "first_difference_simloglm"

    return(result_object)
  }
