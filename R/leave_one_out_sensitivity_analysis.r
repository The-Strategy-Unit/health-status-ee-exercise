# README
# Leave-one-expert-out sensitivity analysis using the final Round 2 estimates
# Repeat the pooling eight times, each time excluding one SME and giving equal
# weight to the remaining seven.

# v.1 – simulation-based approach
# This mirrors the primary analysis. Samples from each included SME’s fitted
# truncated split-normal distribution and combines these to form an equally
# weighted mixture. The pooled P10 and P90 are calculated empirically, with the
# mode estimated using the HSM estimator.

# v.2 – deterministic approach
# Alternative to the simulation-based approach. Constructs the equally weighted
# mixture distribution directly from the fitted truncated split-normal
# distributions, avoiding Monte Carlo simulation. The P10 and P90 are obtained
# from the mixture CDF, the mode is defined as the global maximum of the mixture
# density.

## helpers
source(here::here("R", "process_expert_params.r"))

# load Round 2 results ----
res_r2 <- readxl::read_xlsx(
  here::here("data_raw", "round_2_results_20251126.xlsx")
)

# add identifier
id_lookup <- tibble::tibble(
  email = unique(res_r2$email),
  id = paste0(
    "E", stringr::str_pad(1:8, pad = 0, width = 2, side = "left")
  )
)

# ordering experts in param plots (top to bottom)
id_lookup$id <- factor(
  id_lookup$id,
  levels = sort(unique(id_lookup$id), decreasing = TRUE)
)

res_r2 <- res_r2 |>
  dplyr::left_join(id_lookup, dplyr::join_by(email))

params_r2 <- get_params(res_r2)

# v.1 – simulation-based ----
# summarise a pooled mixture
summarise_mix <- function(x) {
  tibble::tibble(
    p10  = unname(quantile(x, 0.10)),
    mode = as.numeric(modeest::mlv(x, method = "hsm")),
    p90  = unname(quantile(x, 0.90))
  )
}

# pool a set of experts with equal expert weights
pool_experts <- function(df, n_per_expert = 1e5) {

  df |>
    dplyr::group_by(strategy) |>
    dplyr::group_modify(~ {

      vals <- purrr::pmap(
        .x[, c("mu", "sigma_l", "sigma_r")],
        function(mu, sigma_l, sigma_r) {
          rspnorm_trunc(
            n = n_per_expert,
            mode = mu,
            sd1 = sigma_l,
            sd2 = sigma_r
          )
        }
      ) |>
        unlist()

      tibble::tibble(
        p10 = unname(quantile(vals, 0.10)),
        mode = as.numeric(modeest::mlv(vals, method = "hsm")),
        p90 = unname(quantile(vals, 0.90))
      )
    }) |>
    dplyr::ungroup()
}

# leave-one-out sensitivity analysis
n_per_expert <- 1e6
expert_ids <- unique(params_r2$id)
set.seed(014796)

loo_results <- purrr::map_dfr(
  expert_ids,
  function(excluded_id) {

    params_r2 |>
      dplyr::filter(id != excluded_id) |>
      pool_experts(n_per_expert = n_per_expert) |>
      dplyr::mutate(
        analysis = paste0("Exclude ", excluded_id)
      )
  }
)

full_result <- params_r2 |>
  pool_experts(n_per_expert = n_per_expert) |>
  dplyr::mutate(
    analysis = "All 8 SMEs"
  )

v1_sensitivity_results <- dplyr::bind_rows(
  full_result,
  loo_results
) |>
  dplyr::select(
    analysis,
    strategy,
    p10,
    mode,
    p90
  )

# sensitivity table
v1_sensitivity_table <- v1_sensitivity_results |>
  tidyr::pivot_wider(
    names_from = strategy,
    values_from = c(p10, mode, p90),
    names_glue = "{strategy}_{.value}"
  ) |>
  dplyr::select(
    analysis,
    female_p10,
    female_mode,
    female_p90,
    male_p10,
    male_mode,
    male_p90
  )

v1_sensitivity_table |> write.csv(
  file = here::here("data", "v1_leave_one_out_sensitivity_analysis.csv"),
  row.names = FALSE
)




###############################################################################
# ----
###############################################################################
# v.2 - deterministic method ----
psplitnorm <- function(x, mode, sd1, sd2) {

  alpha <- sd1 / (sd1 + sd2)

  ifelse(
    x <= mode,

    # left side
    2 * alpha *
      pnorm((x - mode) / sd1),

    # right side
    alpha +
      2 * (1 - alpha) *
        (pnorm((x - mode) / sd2) - 0.5)
  )
}

psplitnorm_trunc <- function(
  x,
  mode,
  sd1,
  sd2,
  min = 0,
  max = 100
) {

  F_min <- psplitnorm(min, mode, sd1, sd2)
  F_max <- psplitnorm(max, mode, sd1, sd2)
  F_x   <- psplitnorm(x,   mode, sd1, sd2)

  out <- (F_x - F_min) / (F_max - F_min)

  # enforce support
  ifelse(
    x <= min, 0,
    ifelse(x >= max, 1, out)
  )
}

dsplitnorm <- function(x, mode, sd1, sd2) {

  alpha <- sd1 / (sd1 + sd2)

  ifelse(
    x <= mode,

    2 * alpha *
      dnorm((x - mode) / sd1) / sd1,

    2 * (1 - alpha) *
      dnorm((x - mode) / sd2) / sd2
  )
}

dsplitnorm_trunc <- function(
  x,
  mode,
  sd1,
  sd2,
  min = 0,
  max = 100
) {

  F_min <- psplitnorm(min, mode, sd1, sd2)
  F_max <- psplitnorm(max, mode, sd1, sd2)

  norm_const <- F_max - F_min

  out <- dsplitnorm(x, mode, sd1, sd2) / norm_const

  ifelse(x < min | x > max, 0, out)
}

# find global mode of mixture density
find_mix_mode <- function(
  mix_density,
  min = 0,
  max = 100,
  grid_n = 10001
) {

  # evaluate density over fine grid
  grid <- seq(min, max, length.out = grid_n)
  dens <- vapply(grid, mix_density, numeric(1))

  # grid point with highest density
  i <- which.max(dens)

  # if maximum occurs at boundary
  if (i == 1 || i == grid_n) {
    return(grid[i])
  }

  # refine around highest grid point
  optimize(
    f = function(x) -mix_density(x),
    interval = c(grid[i - 1], grid[i + 1])
  )$minimum
}

pool_experts <- function(df, min = 0, max = 100) {

  # equal-weight mixture CDF
  mix_cdf <- function(x) {

    mean(
      mapply(
        function(mu, sl, sr) {
          psplitnorm_trunc(
            x,
            mode = mu,
            sd1 = sl,
            sd2 = sr,
            min = min,
            max = max
          )
        },
        df$mu,
        df$sigma_l,
        df$sigma_r
      )
    )
  }

  # equal-weight mixture density
  mix_density <- function(x) {

    mean(
      mapply(
        function(mu, sl, sr) {
          dsplitnorm_trunc(
            x,
            mode = mu,
            sd1 = sl,
            sd2 = sr,
            min = min,
            max = max
          )
        },
        df$mu,
        df$sigma_l,
        df$sigma_r
      )
    )
  }

  # pooled quantiles
  p10 <- uniroot(
    function(x) mix_cdf(x) - 0.10,
    interval = c(min, max)
  )$root

  p90 <- uniroot(
    function(x) mix_cdf(x) - 0.90,
    interval = c(min, max)
  )$root

  mode <- find_mix_mode(
    mix_density,
    min = min,
    max = max
  )

  tibble::tibble(
    p10 = p10,
    mode = mode,
    p90 = p90
  )
}

leave_one_out <- function(df) {

  expert_ids <- unique(df$id)

  # full analysis
  full <- df |>
    dplyr::group_by(strategy) |>
    dplyr::group_modify(
      ~ pool_experts(.x)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      analysis = "All 8 SMEs",
      .before = 1
    )

  # leave-one-out analyses
  loo <- purrr::map_dfr(
    expert_ids,
    function(excluded_id) {

      df |>
        dplyr::filter(id != excluded_id) |>
        dplyr::group_by(strategy) |>
        dplyr::group_modify(
          ~ pool_experts(.x)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          analysis = paste0("Exclude ", excluded_id),
          .before = 1
        )
    }
  )

  dplyr::bind_rows(full, loo)
}

v2_sensitivity_results <- leave_one_out(params_r2)

v2_sensitivity_table <- v2_sensitivity_results |>
  tidyr::pivot_wider(
    names_from = strategy,
    values_from = c(p10, mode, p90),
    names_glue = "{strategy}_{.value}"
  ) |>
  dplyr::select(
    analysis,
    female_p10,
    female_mode,
    female_p90,
    male_p10,
    male_mode,
    male_p90
  ) |>
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ round(.x, 1)
    )
  )

v2_sensitivity_table |> write.csv(
  file = here::here("data", "v2_leave_one_out_sensitivity_analysis.csv"),
  row.names = FALSE
)