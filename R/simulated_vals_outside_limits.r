# README
# How do we handle simulated values outside [0, 100].

# Three choices for handling simulated values outside [0, 100].
# 1) Truncation: reject and resample any values not in [0, 100]
# 2) Clipping: cap any out-of-range values at 0 or 100
# 3) Squashing: apply a smooth logistic transform so tails compress toward the
# bounds

# Best option is (1) truncation with rejection and resampling. No difference
# between this and filtering values outside the limits except rejection and
# resampling provides the 'correct' number of observations/values.
# see rsplitnorm_truncated.r

# packages
library("dplyr")
library("fanplot")
library("ggplot2")

# helpers
source(here::here("R", "estimate_splitnorm_from_p10p90.r"))
source(here::here("R", "splitnorm_switch_params.r"))
source(here::here("R", "rsplitnorm_truncated.r"))


# compare original v. truncation v. filter
p <- est_spnorm_from_p10p90(mode = 45, p10 = 40, p90 = 90)

x <- fanplot::rsplitnorm(
  n = 1e5,
  mode = p$mu,
  sd1 = p$sigma_l,
  sd2 = p$sigma_r
)

y <- rspnorm_trunc(
  n = 1e5,
  mode = p$mu,
  sd1 = p$sigma_l,
  sd2 = p$sigma_r
)

df_x <- tibble::tibble(val = x)
df_y <- tibble::tibble(val = y)

# plot
ggplot2::ggplot() +
  # reject and resample
  ggplot2::geom_density(
    ggplot2::aes(x = val),
    data = df_y,
    color = "gold",
    linewidth = 2,
  ) +
  # simple filter
  ggplot2::geom_density(
    ggplot2::aes(x = val),
    data = df_x |> dplyr::filter(val >= 0, val <= 100),
    color = "red",
    linewidth = .5
  ) +
  # original
  ggplot2::geom_density(
    ggplot2::aes(x = val),
    data = df_x,
    color = "#b2b2b2",
    linewidth = 1
  ) +
  ggplot2::scale_x_continuous(limits = c(0, 150))

max(df_y$val) # truncated
max(df_x$val) # original

# Why the appearance of a turnpoint close to 100?
# ggplot2::geom_density() does not compute truncated densities. It performs
# kernel density estimation (KDE) on a sample. KDE always produces a smooth
# curve, so even if the data are truncated (e.g., all values <= 100), the
# density estimate:
# ~ tapers smoothly toward the boundary
# ~ never forms a vertical cliff
# ~ spreads slightly beyond the truncation limit unless you manually constrain
# x-axis limits

# In order to remove the appearance of a turnpoint we can compute and plot the
# theoretical PDF (instead of the KDE)
a <- 0L
b <- 100L

# compute the theoretical (unnormalized) two-piece density
split_pdf <- function(x, mu = 0, sigma_l = 1, sigma_r = 1) {
  c <- sqrt(2 / pi) / (sigma_l + sigma_r)
  ifelse(
    x < mu,
    c * exp(- (x - mu)^2 / (2 * sigma_l^2)),
    c * exp(- (x - mu)^2 / (2 * sigma_r^2))
  )
}

# normalization constant (mass inside [a, b])
# integrate numerically (or compute analytically by splitting at mu)
xs <- seq(a - 1, b + 1, length.out = 2000)
fx <- split_pdf(xs, mu = p$mu, sigma_l = p$sigma_l, sigma_r = p$sigma_r)
mass_in_interval <- sum(fx[xs >= a & xs <= b]) * (xs[2] - xs[1])

# truncated (theoretical) pdf on a grid
grid <- seq(a - 0.5, b + 0.5, length.out = 800)
theo_pdf <- split_pdf(
  grid,
  mu = p$mu,
  sigma_l = p$sigma_l,
  sigma_r = p$sigma_r
)
theo_pdf[!(grid >= a & grid <= b)] <- 0
theo_pdf <- theo_pdf / mass_in_interval

df_plot <- data.frame(x = grid, theo = theo_pdf)

ggplot2::ggplot() +
  # reject and resample: KDE
  ggplot2::geom_density(
    ggplot2::aes(x = val),
    data = df_y,
    color = "gold",
    linewidth = 2,
  ) +
  # reject and resample: true PDF
  ggplot2::geom_line(
    ggplot2::aes(x = x, y = theo),
    data = df_plot,
    color = "red",
  ) +
  # original
  ggplot2::geom_density(
    ggplot2::aes(x = val),
    data = df_x,
    color = "#b2b2b2",
    linewidth = 1
  ) +
  # ggplot2::scale_x_continuous(limits = c(0, 100)) # nolint: commented_code_linter, line_length_linter.
  ggplot2::scale_x_continuous(limits = c(0, 150))
