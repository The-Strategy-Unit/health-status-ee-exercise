# README
# Estimating a split normal distribution from mode, p10 and p90

# packages
library("dplyr")
library("fanplot")
library("ggplot2")


# estimate split normal from mode, p10 and p90 ----
# param: mode, type: real, most likely value
# param: p10, type: real, 10th percentile value (p10 < mode)
# param: p90, type: real, 90th percentile value (mode < p90)
# param: tol, type: real +ve, convergence tolerance
# returns: list with split normal parameters (mode, sd1, sd2), rtype: list
est_spnorm_from_p10p90 <- function(mode, p10, p90, tol = 1e-10) {

  # check supplied arguments are valid
  stopifnot(is.finite(mode), is.finite(p10), is.finite(p90))

  if (!(p10 < mode && mode < p90))
    stop("Fn requires p10 < mode < p90")

  md <- mode # avoid clash with base R fn
  # ensure numerical stability
  eps <- 1e-6 # epsilon (arbitrarily small positive value)

  # alpha must satisfy: 0.1/(2*alpha) ∈ (0, 0.5) and RHS ∈ (0.5, 1)
  lower <- max(0.1 + eps, eps)
  upper <- min(0.90 - eps, 1 - eps)

  # fn to estimate alpha (share of total scale allocated to the left side)
  f <- function(alpha) {
    # implied z-scores
    z_l <- qnorm(0.1 / (2 * alpha)) # < 0
    z_r <- qnorm(0.5 + (0.90 - alpha) / (2 * (1 - alpha))) # > 0

    # convert to sigmas
    sigma_l <- (md - p10) / -z_l
    sigma_r <- (p90 - md) / z_r

    # fixed-point residual: implied alpha_hat - alpha
    alpha_hat <- sigma_l / (sigma_l + sigma_r)
    alpha_hat - alpha
  }

  # bisection for alpha
  r <- uniroot(f, c(lower, upper), tol = tol)
  alpha <- r$root

  # return split normal parameters
  z_l <- qnorm(0.1 / (2 * alpha))
  z_r <- qnorm(0.5 + (0.90 - alpha) / (2 * (1 - alpha)))
  sigma_l <- (md - p10) / -z_l
  sigma_r <- (p90 - md) / z_r

  list(
    mu = md,
    sigma_l = sigma_l,
    sigma_r = sigma_r,
    alpha = alpha,
    check = c(
      f_p10  = 2 * alpha * pnorm((p10 - md) / sigma_l),
      f_p90 = alpha + 2 * (1 - alpha) * (pnorm((p90 - md) / sigma_r) - 0.5)
    )
  )
}

# example ----
test_md  <- 0.5
test_p10  <- 0.44
test_p90 <- 0.7

fit <- est_spnorm_from_p10p90(mode = test_md, p10 = test_p10, p90 = test_p90)

# compare with fanplot quantile fn
fanplot::qsplitnorm(0.1, mode = fit$mu, sd1 = fit$sigma_l, sd2 = fit$sigma_r)
fanplot::qsplitnorm(0.90, mode = fit$mu, sd1 = fit$sigma_l, sd2 = fit$sigma_r)

# plot fit
x <- fanplot::rsplitnorm(
  1e5,
  mode = fit$mu,
  sd1 = fit$sigma_l,
  sd2 = fit$sigma_r
)

xdat <- data.frame(x = x)

ggplot2::ggplot(xdat) +
  ggplot2::geom_density(ggplot2::aes(x = x)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = test_md), linetype = "43") +
  ggplot2::geom_vline(ggplot2::aes(xintercept = test_p10), linetype = "43") +
  ggplot2::geom_vline(ggplot2::aes(xintercept = test_p90), linetype = "43")
