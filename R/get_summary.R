#' Summarize a simloglm object
#'
#' @param input_obj An object of class simloglm (the output of calling simloglm).
#' @param which_qoi Are you interested in the conditional geometric mean (which
#'   is also the conditional median, `"median"`) or the conditional arithmetic
#'   mean (`"mean"`)? Default is `"median"`.
#' @param alpha The significance level for the resulting confidence interval.
#'   Default is 0.05.
#' @return A list of class "summary_simloglm" with the point estimate for every
#'   scenario and the corresponding simulation quantiles.
#' @export
#'
#' @examples
#' df <- cars
#' regression <- lm(log(dist) ~ speed, data = df)
#' # Specifying no scenario simulates at the mean of speed.
#' simulation_results <- simloglm(regression, verbose = FALSE)
#' simulation_summary <- get_summary(simulation_results)
#' simulation_summary

get_summary <-
  function(input_obj,
           which_qoi = "median",
           alpha = 0.05) {
    qoi <- check_simloglm(input_obj, which_qoi)

    point_estimate <- input_obj[[paste0(which_qoi, "_point_estimate")]]

    quantiles <- apply(qoi, 2, stats::quantile, c(alpha / 2, 1 - alpha / 2))

    result_object <- list(point_estimate = point_estimate,
                          quantiles = quantiles)

    class(result_object) <- "summary_simloglm"

    return(result_object)
  }

# Shared validation: the object must be a simloglm and which_qoi must name one
# of the two matrices it carries.
check_simloglm <- function(input_obj, which_qoi, min_scenarios = 1L) {
  if (!inherits(input_obj, "simloglm")) {
    stop("The input object needs to be of class simloglm.", call. = FALSE)
  }

  if (length(which_qoi) != 1L || !which_qoi %in% c("median", "mean")) {
    stop('which_qoi must be either "median" or "mean".', call. = FALSE)
  }

  qoi <- input_obj[[which_qoi]]

  if (ncol(qoi) < min_scenarios) {
    stop(
      "This quantity needs at least two scenarios. If more than two scenarios ",
      "are passed please specify which should be used via which_scenarios.",
      call. = FALSE
    )
  }

  qoi
}

# Pick out two scenario columns and check the selection is usable.
pick_scenarios <- function(qoi, which_scenarios) {
  if (length(which_scenarios) != 2L) {
    stop("which_scenarios must name exactly two scenarios.", call. = FALSE)
  }
  if (any(which_scenarios < 1L) || any(which_scenarios > ncol(qoi))) {
    stop(
      "which_scenarios must be between 1 and ", ncol(qoi), ".",
      call. = FALSE
    )
  }

  qoi[, which_scenarios, drop = FALSE]
}
