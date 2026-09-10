#' Plot function for a simloglm object
#'
#' Plots the point estimate for every scenario together with its simulation
#' interval.
#'
#' @param x An object of class simloglm (the output of calling simloglm).
#' @param which_qoi Which quantity should be plotted, the conditional geometric
#'   mean (`"median"`) or the conditional arithmetic mean (`"mean"`)? Defaults to
#'   `"median"`.
#' @param alpha Set the significance level. Default is 0.05.
#' @param twosided If `TRUE` (the default) an equal-tailed interval holding
#'   `1 - alpha` of the simulations is drawn. If `FALSE` the two one-sided bounds
#'   of level `1 - alpha` are drawn instead, i.e. the `alpha` and `1 - alpha`
#'   quantiles.
#' @param xlab,ylab,main Passed to the underlying plot; sensible defaults are
#'   derived from the object.
#' @param ... Further graphical parameters passed to [graphics::plot()].
#' @return A data frame with the plotted point estimates and interval bounds,
#'   invisibly.
#' @export
#'
#' @examples
#' df <- cars
#' regression <- lm(log(dist) ~ speed, data = df)
#' plot(simloglm(regression, scenario = list(speed = c(5, 10, 20)),
#'               verbose = FALSE))

plot.simloglm <- function(x,
                          which_qoi = "median",
                          alpha = 0.05,
                          twosided = TRUE,
                          xlab = "Scenario",
                          ylab = NULL,
                          main = NULL,
                          ...) {
  qoi <- check_simloglm(x, which_qoi)

  probs <- if (twosided) {
    c(alpha / 2, 1 - alpha / 2)
  } else {
    c(alpha, 1 - alpha)
  }

  bounds <- apply(qoi, 2, stats::quantile, probs)
  point <- as.vector(x[[paste0(which_qoi, "_point_estimate")]])

  labels <- colnames(qoi)
  if (is.null(labels)) {
    labels <- paste0("scenario_", seq_along(point))
  }

  if (is.null(ylab)) {
    ylab <- if (isTRUE(x$logged_dv)) {
      if (which_qoi == "median") {
        "Conditional geometric mean of Y"
      } else {
        "Conditional arithmetic mean of Y"
      }
    } else {
      "Conditional mean of Y"
    }
  }

  if (is.null(main)) {
    main <- paste0(
      if (isTRUE(x$observed_value_approach)) "Observed value" else "Average case",
      " predictions, ",
      round(100 * (1 - alpha)), "% interval"
    )
  }

  at <- seq_along(point)
  ylim <- range(bounds, point, finite = TRUE)

  graphics::plot(
    at,
    point,
    type = "n",
    xlim = c(0.5, length(point) + 0.5),
    ylim = ylim,
    xaxt = "n",
    xlab = xlab,
    ylab = ylab,
    main = main,
    ...
  )
  graphics::axis(1, at = at, labels = labels)
  graphics::segments(at, bounds[1, ], at, bounds[2, ], lwd = 2)
  graphics::points(at, point, pch = 19)

  invisible(data.frame(
    scenario = labels,
    point_estimate = point,
    lower = bounds[1, ],
    upper = bounds[2, ],
    row.names = NULL,
    stringsAsFactors = FALSE
  ))
}
