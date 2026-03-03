# README
# In process_expert_params.r we estimate a mixture disribution for the QOI in
# 2045. We also require to know the distribution of the QOI for each year from
# 2022 to at least 2045, and perhaps beyond for use in the NHP model.

# IMPORTANT!: the most recent DFLE data is for the three-year period 2020-2022.
# Experts are asked for their view of the QOI in 2045, so we need to plot a path
# from 2021 to 2045.

# 1) For 'n' sets of expert parameters (p10, p90, mode) for the QOI in 2045, and
# for both sexes, linearly interpolate the value of the parameters for each
# intervening year. Fix the base year (2021) QOI value as the most recent
# observed value, assume zero uncertainty (sd = 0) in the base year; assume the
# skew (balance of risk) remains constant over time.
# 2) This gives 'n' sets of parameters, for both sexes, for each year from 2021
# to 2045. For each year, pool simulated values from each of the 'n' split
# normal distributions to obtain a mixture distribution for that year. This
# gives a single mixture distribution for each year. In the absence of a closed
# form for the mixture of split normal distributions, we need to be able to
# reproduce the simulated values from the mixtures.

# We need to run a lot of simulations (10 experts x 2 f|m x 30 years = 600) and
# rspnorm_trunc() uses a while loop so is slow. Est. time 2m for 600 simulations
# of 1e4 values.

# packages
library("dplyr")
library("here")
library("purrr")
library("tibble")
library("tidyr")

# helpers
source(here::here("R", "splitnorm_switch_params.r"))
source(here::here("R", "rsplitnorm_truncated.r"))

# parameters
base_yr <- 2021L
qoi_yr <- 2045L
end_yr <- 2050L
qoi_2019_f <- 51.01 # double-check this value is accurate
qoi_2019_m <- 56.88 # double-check this value is accurate


# grid (years x sex x experts) ----
make_grid <- function(df_spnorm_params) {
  df_spnorm_params |>
    dplyr::select(email, strategy) |>
    dplyr::group_by(email, strategy) |>
    expand(year = seq.int(base_yr, end_yr, 1)) |>
    dplyr::arrange(email, strategy, year)
}

# interpolate params ----
interp_params <- function(df_grid, df_spnorm_params) {
  df_grid |>
    dplyr::left_join(
      df_spnorm_params |>
        dplyr::select(
          email, strategy,
          mu, sigma_l, sigma_r
        ) |>
        # add alternative params
        dplyr::mutate(
          sd = (spnorm_switch_params(mu, sigma_l, sigma_r)$sd),
          skew = (spnorm_switch_params(mu, sigma_l, sigma_r)$skew)
        ) |>
        dplyr::mutate(year = qoi_yr),
      dplyr::join_by(email, strategy, year)
    ) |>
    dplyr::select(-sigma_l, -sigma_r) |>
    dplyr::mutate(
      # set base year mode
      mu = dplyr::case_when(
        year == base_yr & strategy == "female" ~ qoi_2019_f,
        year == base_yr & strategy == "male" ~ qoi_2019_m,
        .default = mu
      ),
      # set base year sd
      sd = dplyr::if_else(year == base_yr, 0, sd)
    ) |>
    # fix skew constant, by expert & sex
    dplyr::group_by(email, strategy) |>
    tidyr::fill(skew, .direction = "downup") |>
    tidyr::nest() |>
    # interpolate params for missing years, by expert
    dplyr::mutate(
      lm_mode = purrr::map(data, \(x) lm(mu ~ year, data = x)),
      lm_sd = purrr::map(data, \(x) lm(sd ~ year, data = x))
    ) |>
    dplyr::mutate(
      fit_mode = purrr::map2(lm_mode, data, \(x, y) predict(x, newdata = y)),
      fit_sd = purrr::map2(lm_sd, data, \(x, y) predict(x, newdata = y))
    ) |>
    dplyr::ungroup() |>
    # fix skew constant
    dplyr::mutate(fit_skew = purrr::map(data, \(x) x$skew)) |>
    # retain year
    dplyr::mutate(year = purrr::map(data, \(x) x$year)) |>
    dplyr::select(-data, -tidyselect::starts_with("lm"))
}

# simulate split normals ----
simul_interp <- function(df_interp_params, interp_n = 1e5) {
  df_interp_params |>
    tidyr::unnest(cols = c(tidyselect::starts_with("fit"), "year")) |>
    # simulate from params
    dplyr::rowwise() |>
    # dplyr::if_else() expects vectors and won't work with lists
    dplyr::mutate(
      sim_vals = list(
        if (year == base_yr) {
          # don't simulate for base year, just repeat mode
          rep(fit_mode, interp_n)
        } else {
          # use truncated version
          rspnorm_trunc(
            n = interp_n,
            mode = fit_mode,
            sd = fit_sd,
            skew = fit_skew
          )
        }
      )
    ) |>
    dplyr::ungroup()
}

# create mixture for each year ----
mix_interp <- function(df_spnorm_simul) {
  df_spnorm_simul |>
    dplyr::group_by(year, strategy) |>
    dplyr::summarise(
      mix_vals = list(
        unlist(sim_vals)
      )
    ) |>
    dplyr::ungroup()
}