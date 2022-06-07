#' Get the ratio between two scenarios from a simloglm object
#'
#' @param input_obj An object of class simloglm (the output of calling simloglm). With at least two scenarios.
#' @param which_qoi Are you interested in the median or mean ? Default is "median".
#' @param which_scenarios If you pass a simloglm object with more than two scenarios please specify which scenarios should be used. Default is c(1, 2).
#' @param alpha The significance level for the resulting confidence interval. Default is 0.05.
#' @export
#'
#' @examples
#' df <- cars
#' regression <- lm(log(dist)~speed, data = df)
#' # Explicitily specifying two scenarios.
#' simulation_results <- simloglm(regression, scenario = list(speed = c(5, 20)))
#' ratio_summary <- get_ratio(simulation_results)
#' ratio_summary

get_ratio <-
  function(input_obj,
           which_qoi = "median",
           which_scenarios = c(1, 2),
           alpha = 0.05) {
    if (class(input_obj) == "simloglm") {
      if (ncol(input_obj[[paste0(which_qoi)]]) >= 2) {

        point_estimate <-
          input_obj[[paste0(which_qoi,"_point_estimate")]][which_scenarios]

        ratio_fct <- function(x){
          x[2]/x[1]
        }

        ratio <- ratio_fct(point_estimate)

        ratio_dist <-
          matrix(apply(input_obj[[paste0(which_qoi)]][,which_scenarios,], 1, ratio_fct))
        quantiles <-
          apply(ratio_dist, 2, stats::quantile, c(alpha / 2, 1 - alpha /
                                                 2))


        result_object <- list(ratio = ratio,
                              quantiles = quantiles)


        class(result_object) <- "ratio_simloglm"

        return(result_object)
      } else {
        stop("The ratio can only be calculated with at least two scenarios. If more than two scenarios are passed please specify which should be used to calculate the ratio.")
      }
    } else {
      stop("The input object needs to be of class simloglm.")
    }

  }
