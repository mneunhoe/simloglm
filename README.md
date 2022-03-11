
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simloglm

<!-- badges: start -->
<!-- badges: end -->

The goal of simloglm is to provide functions to simulate the correct
quantities of interest from linear regression models with logged
dependent variables. This paper accompanies the paper “How to Improve
the Substantive Interpretation of Regression Results when the Dependent
Variable is logged”.

## Installation

Currently you can install the development version of simloglm from from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mneunhoe/simloglm")
```

## Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(simloglm)
df <- cars
regression <- lm(log(dist)~speed, data = df)
# Specifiying no scenario to simulate at the mean of speed.
simulation_results_average <- simloglm(regression)
#> No scenario provided, all variables were set to their mean.
# Explicitily specifying a scenario.
simulation_results_scenario <- simloglm(regression, scenario = list(speed = c(5, 10, 20)))
```

See the vignettes for replications of published work.

## Issues

Please reach out in case you find bugs and open an issue on Github.
