#' Summarize a simloglm object
#'
#' @param input_obj An object of class simloglm (the output of calling simloglm).
#' @param which_mean Are you interested in the conditional arithmetic mean ("arithmetic_mean") or conditional geometric mean ("geometric_mean"; same as median)? Default is "geometric_mean".
#' @param alpha The significance level for the resulting confidence interval. Default is 0.05.
#' @export
#'
#' @examples
#' df <- cars
#' regression <- lm(log(dist)~speed, data = df)
#' # Specifiying no scenario to simulate at the mean of speed.
#' simulation_results <- simloglm(regression)
#' simulation_summary <- get_summary(simulation_results)
#' simulation_summary

get_summary <-
  function(input_obj,
           which_qoi = "median",
           alpha = 0.05) {
    if (class(input_obj) == "simloglm") {
      point_estimate <-
        input_obj[[paste0(which_qoi,"_point_estimate")]]

      quantiles <-
        apply(input_obj[[paste0(which_qoi)]], 2, stats::quantile, c(alpha / 2, 1 - alpha /
                                                                       2))


      result_object <- list(point_estimate = point_estimate,
                            quantiles = quantiles)


      class(result_object) <- "summary_simloglm"

      return(result_object)
    } else {
      stop("The input object needs to be of class simloglm.")
    }

  }
