
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simloglm

<!-- badges: start -->
<!-- badges: end -->

The goal of simloglm is to provide functions to simulate the correct
quantities of interest from linear regression models with logged
dependent variables. This package accompanies the paper “How to Improve
the Substantive Interpretation of Regression Results when the Dependent
Variable is logged”.

## Installation

Currently you can install the development version of simloglm from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mneunhoe/simloglm")
```

## Usage

``` r
library(simloglm)
df <- cars
regression <- lm(log(dist) ~ speed, data = df)

# Specifying no scenario simulates at the mean of every variable.
simulation_results_average <- simloglm(regression, verbose = FALSE)

# Explicitly specifying a scenario.
simulation_results_scenario <- simloglm(regression,
                                        scenario = list(speed = c(5, 10, 20)),
                                        verbose = FALSE)

get_summary(simulation_results_scenario, which_qoi = "median")
#> $point_estimate
#>  speed = 5 speed = 10 speed = 20 
#>   9.776186  17.881656  59.825217 
#> 
#> $quantiles
#>       speed = 5 speed = 10 speed = 20
#> 2.5%    7.22159   14.63705   50.52883
#> 97.5%  12.84772   21.27112   71.01941
#> 
#> attr(,"class")
#> [1] "summary_simloglm"
get_summary(simulation_results_scenario, which_qoi = "mean")
#> $point_estimate
#>  speed = 5 speed = 10 speed = 20 
#>   10.80009   19.75449   66.09101 
#> 
#> $quantiles
#>       speed = 5 speed = 10 speed = 20
#> 2.5%    8.04516   16.31519   56.53885
#> 97.5%  14.43019   24.05435   80.11661
#> 
#> attr(,"class")
#> [1] "summary_simloglm"
```

`median` holds the conditional geometric mean (which is also the
conditional median) of the dependent variable, `mean` the conditional
arithmetic mean, each as a matrix with one row per simulation draw and
one column per scenario.

Scenario values are given on the **original scale** of the variables, so
a transformation in the formula is applied for you:

``` r
loglog <- lm(log(dist) ~ log(speed), data = df)
res <- simloglm(loglog, scenario = list(speed = c(10, 11)), verbose = FALSE)

# A ten percent increase in speed multiplies the median by 1.1^beta.
get_ratio(res)$ratio
#> [1] 1.165004
```

Variables you leave out of the scenario are held at their mean (numeric)
or reference level (factor). See `vignette("log_log_models")` for
elasticities and first differences, and the other vignettes for
replications of published work.

## Issues

Please reach out in case you find bugs and open an issue on Github.
