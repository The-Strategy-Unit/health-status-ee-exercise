# README
# At the workshop on 26-Nov 2025 params (p10, p90, best) will be collected via
# Rhian's app from 5-10 participants (the experts). You need to be logged into
# the Connect server for the download results link to work.

# IMPORTANT!:
# there will be 2 phases/rounds of elicitation in the workshop
# there will be 2 sets of params (F|M)
# ultimately we need to ensure any simulated distribution used for the NHP model
# is completely reproducible starting from the params obtained in the workshop
# [set.seed()].

# packages
library("dplyr")
library("here")
library("purrr")
library("stringr")
library("tidyr")
library("uuid")

# helpers
source(here::here("R", "estimate_splitnorm_from_p10p90.r"))
source(here::here("R", "rsplitnorm_truncated.r"))

# parameters
expert_n <- 8 # no. of experts
# parameter constraints
# min_p10 <= p10 < best < p90 <= max_p90
min_p10 <- 0L
max_p90 <- 100L


# dummy data from app ----
# [email, timestamp, which_phase, strategy, lo, hi, mode, comments_lo, comments_hi] # nolint: line_length_linter.
test_dummy <- expand.grid(
  email = uuid::UUIDgenerate(n = expert_n),
  which_phase = c(1L, 2L),
  strategy = c("female", "male")
) |>
  dplyr::arrange(which_phase, strategy, email) |>
  dplyr::mutate(
    timestamp = seq(
      from = as.POSIXct(Sys.time()),
      by   = "s",
      length.out = n()
    )
  )

set.seed(796)
test_dummy <- test_dummy |>
  dplyr::mutate(
    lo = sample(min_p10:(max_p90 - 3), n(), replace = TRUE),
    hi = purrr::map_int(lo, \(x) sample((x + 2):max_p90, 1)),
    mode = purrr::map2_int(lo, hi, \(x, y) {
      if ((y - x) <= 2) return(x + 1) # fallback if range too small
      sample((x + 1):(y - 1), 1)
    })
  )

test_dummy <- test_dummy |>
  dplyr::mutate(
    comments_lo = "Lorem ipsum dolor sit amet",
    comments_hi = "Lorem ipsum dolor sit amet"
  )

# app returns separate df for each phase
test_dummy <- split(test_dummy, test_dummy$which_phase)

# estimate split normal params ----
get_params <- function(df_res) {
  df_res |>
    # estimate split normal parameters
    dplyr::rowwise() |>
    dplyr::mutate(
      splitnorm_params = list(
        est_spnorm_from_p10p90(mode, lo, hi)
      )
    ) |>
    tidyr::unnest_wider(splitnorm_params) |>
    dplyr::ungroup()
}

# simulate split normal distributions ----
get_simul <- function(df_params, simul_n = 1e5) {
  df_params |>
    # simulate from params
    dplyr::rowwise() |>
    dplyr::mutate(
      sim_vals = list(
        # use truncated version
        rspnorm_trunc(
          simul_n,
          mode = mu,
          sd1 = sigma_l,
          sd2 = sigma_r
        )
      )
    ) |>
    dplyr::ungroup()
}

# create mixture distribution ----
# use r2 results!
get_mix <- function(df_simul, mix_n = 1e5) {
  df_simul |>
    # simulate from params
    dplyr::rowwise() |>
    dplyr::mutate(
      sim_vals = list(
        # use truncated version
        rspnorm_trunc(
          mix_n,
          mode = mu,
          sd1 = sigma_l,
          sd2 = sigma_r
        )
      )
    ) |>
    dplyr::ungroup() |>
    # combine ALL simulated dists into a single mixture
    dplyr::select(which_phase, strategy, sim_vals) |>
    tidyr::unnest_longer(sim_vals) |>
    dplyr::rename(mix_vals = sim_vals)
}