# README
# Convert from a split normal parameterized by (mode, sd1, sd2) to an
# alternative parameterization of (mode, sd, skew). These parameterizations are
# consistent with the arguments used by fanplot::rsplitnorm()

# packages
library("dplyr")
library("fanplot")
library("ggplot2")


# switch split normal parameters ----
# param: mode, type: real, most likely value
# param: sd1, type: real, standard deviation left-hand side
# param: sd2, type: real, standard deviation right-hand side
# returns: list with new parameters (mode, sd, skew), rtype: list
spnorm_switch_params <- function(mode, sd1, sd2) {

  # uncertainty indicator
  var1 <- sd1^2
  var2 <- sd2^2
  var0 <- 2 / (1 / var1 + 1 / var2)
  sd <- sqrt(var0)

  # normalized skewness indicator (-1, 1)
  skew <- (var2 - var1) / (var2 + var1)

  list(
    mode = mode,
    sd = sd,
    skew = skew
  )
}

# example ----
# check both parameterizations are mathematically equivalent
set.seed(796)
x <- fanplot::rsplitnorm(n = 1e4, mode = 50, sd1 = 4, sd2 = 8)

xdat <- data.frame(x = x)

switch_params <- spnorm_switch_params(mode = 50, sd1 = 4, sd2 = 8)

set.seed(796)
y <- fanplot::rsplitnorm(
  n = 1e4,
  mode = switch_params$mode,
  sd = switch_params$sd,
  skew = switch_params$skew
)

ydat <- data.frame(x = y)

ggplot2::ggplot() +
  ggplot2::geom_density(
    ggplot2::aes(x = x),
    color = "gold",
    linewidth = 3,
    data = ydat
  ) +
  ggplot2::geom_density(
    ggplot2::aes(x = x),
    color = "#b2b2b2",
    linewidth = 1,
    data = xdat
  )
