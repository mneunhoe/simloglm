#' Get the first difference from a simloglm object
#'
#' @param input_obj An object of class simloglm (the output of calling simloglm). With two scenarios.
#' @param which_mean Are you interested in the conditional arithmetic mean ("arithmetic_mean") or conditional geometric mean ("geometric_mean"; same as median)? Default is "geometric_mean".
#' @param alpha The significance level for the resulting confidence interval. Default is 0.05.
#' @export
#'
#' @examples
#' df <- cars
#' regression <- lm(log(dist)~speed, data = df)
#' # Explicitily specifying two scenarios.
#' simulation_results <- simloglm(regression, scenario = list(speed = c(5, 20)))
#' first_difference_summary <- get_first_difference(simulation_results)
#' first_difference_summary

get_first_difference <-
  function(input_obj,
           which_mean = "geometric_mean",
           alpha = 0.05) {
    if (class(input_obj) == "simloglm") {
      if (ncol(input_obj[[paste0(which_mean)]]) == 2) {
        gm <-
          apply(input_obj[[paste0(which_mean)]], 2, function(x)
            exp(mean(log(x))))

        fd <- diff(gm)

        fd_dist <-
          matrix(apply(input_obj[[paste0(which_mean)]], 1, diff))
        quantiles <-
          apply(fd_dist, 2, stats::quantile, c(alpha / 2, 1 - alpha /
                                          2))


        result_object <- list(fd = fd,
                              quantiles = quantiles)


        class(result_object) <- "first_difference_simloglm"

        return(result_object)
      } else {
        stop("The first difference can only be calculated with two scenarios.")
      }
    } else {
      stop("The input object needs to be of class simloglm.")
    }

  }
