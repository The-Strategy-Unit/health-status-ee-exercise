# README
# Review results from expert elicitation workshop (held 26 November 2025).
# Comparison with previous elicitation exercise:
# ONS data on health state life expectancies
# Nov-25 workshop - March 2024 release was current
# Oct-22 workshop - March 2022 release was current
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyallagesuk/current # nolint: line_length_linter.

# packages
library("dplyr")
library("fanplot")
# devtools::install_github("jasonhilton/ggfan") # nolint: commented_code_linter.
library("ggfan")
library("ggplot2")
library("ggridges")
library("here")
library("readxl")
library("MetBrewer")
library("see")
library("tibble")
library("tidyr")

# helpers
source(here::here("R", "process_expert_params.r"))
source(here::here("R", "interpolate_expert_params.r"))
source(here::here("R", "plot_fns.r"))
source(here::here("R", "theme_hsee_exercise.r"))
source(here::here("R", "font_hoist.r"))
font_hoist("Fira Sans")
theme_hseee <- theme_hseee()
ggplot2::theme_set(theme_hseee)

# palettes
# cat(paste(" ", MetBrewer::met.brewer("Egypt", n = 2)))
# pal_sex <- c("#dd5129", "#0f7ba2") # [f|m]
# MetBrewer::display_all(n = 8, colorblind_only = TRUE) # pick one of these [Hiroshige] # nolint: line_length_linter.
# # https://github.com/BlakeRMills/MetBrewer
# cat(paste(" ", MetBrewer::met.brewer("Tam", n = 3))) # [lo|mode|hi]
# pal_params <- c("#ffd353", "#ef8737", "#bb292c")
# okabeito_colors() # nolint: commented_code_linter.
# https://easystats.github.io/see/reference/scale_color_okabeito.html
pal_params <- c("#56B4E9", "#F5C710", "#56B4E9")
pal_sex <- c("#D55E00", "#0072B2")


# read results ----
res_r1 <- readxl::read_xlsx(
  here::here("data_raw", "round_1_results_20251126.xlsx")
)

res_r2 <- readxl::read_xlsx(
  here::here("data_raw", "round_2_results_20251126.xlsx")
)

# add identifier
id_lookup <- tibble::tibble(
  email = unique(res_r1$email),
  id = paste0(
    "E", stringr::str_pad(1:8, pad = 0, width = 2, side = "left")
  )
)

# ordering experts in param plots (top to bottom)
id_lookup$id <- factor(
  id_lookup$id,
  levels = sort(unique(id_lookup$id), decreasing = TRUE)
)

res_r1 <- res_r1 |>
  dplyr::left_join(id_lookup, dplyr::join_by(email))

res_r2 <- res_r2 |>
  dplyr::left_join(id_lookup, dplyr::join_by(email))

# plot r1/r2 params ----
prep_params <- function(df) {
  df |>
    dplyr::select(id, lo:mode, which_phase, strategy) |>
    tidyr::pivot_longer(
      cols = lo:mode,
      names_to = "param",
      values_to = "value"
    ) |>
    dplyr::mutate(
      param = factor(
        param,
        levels = c("lo", "mode", "hi")
      )
    )
}

p1_dat <- prep_params(res_r1)
p2_dat <- prep_params(res_r2)

p1 <- plot_params(p1_dat)
p2 <- plot_params(p2_dat)

ggsave(
  here::here("figures", "workshop_r1_params.png"),
  p1,
  width = 220, height = 165, units = c("mm")
)

ggsave(
  here::here("figures", "workshop_r2_params.png"),
  p2,
  width = 220, height = 165, units = c("mm")
)

# plot r1/r2 dists ----
params_r1 <- get_params(res_r1)
params_r2 <- get_params(res_r2)

simul_r1 <- get_simul(params_r1, simul_n = 1e5)
simul_r2 <- get_simul(params_r2, simul_n = 1e5)

p3 <- plot_pdfs(simul_r1)
p4 <- plot_pdfs(simul_r2)

ggsave(
  here::here("figures", "workshop_r1_pdfs.png"),
  p3,
  width = 220, height = 165, units = c("mm")
)

ggsave(
  here::here("figures", "workshop_r2_pdfs.png"),
  p4,
  width = 220, height = 165, units = c("mm")
)

# plot r2 mixture ----
mix_r2 <- get_mix(params_r2, mix_n = 1e5)

p5 <- plot_mix(mix_r2, simul_r2)

ggsave(
  here::here("figures", "workshop_r2_mixture.png"),
  p5,
  width = 220, height = 165, units = c("mm")
)

# plot comparison with prev. version ----
qoi_2021 <- list(female = 51.01, male = 56.88)
# previous exercise took place in October 2022, baseline data was for the
# period 2018-2020.
# 52.57% is taken directly from ONS published spreadsheet; this is slightly
# different to the value I manually calculated at the time of the first workshop
# using data supplied by Chris White from the raw data files (46.69% & 52.73%).
qoi_2019_old <- list(female = .4667 * 100, male = .5257 * 100)
qoi_2019_new <- list(female = 51.55, male = 56.64)

# output from 2022 workshop was a single gamma distribution
# values from C:/Projects/hsa_inputs/
gam_lim <- 0.44
gam_shp <- 2.36
gam_rt <- 50.6
gam_mod <- (gam_lim + ((gam_shp - 1) / gam_rt)) * 100

# multiplier to derive f/m distributions
# values from C:/Projects/hsa_inputs/
mx_f <- 0.939
mx_m <- 1.061

qoi_2035f <- gam_mod * mx_f
qoi_2035m <- gam_mod * mx_m

# gamma from workshop
f_vals <- (gam_lim + rgamma(n = 1e5, shape = gam_shp, rate = gam_rt)) * mx_f
m_vals <- (gam_lim + rgamma(n = 1e5, shape = gam_shp, rate = gam_rt)) * mx_m

# split normal approximation of gamma
# values from C:/Projects/hsa_inputs/
f_vals <- fanplot::rsplitnorm(
  n = 1e5,
  mode = gam_mod / 100,
  sd = 0.01894423,
  skew = 0.7472889
) * mx_f

m_vals <- fanplot::rsplitnorm(
  n = 1e5,
  mode = gam_mod / 100,
  sd = 0.01894423,
  skew = 0.7472889
) * mx_m

wshop_22 <- tibble::tibble(vals = f_vals, sex = "female") |>
  dplyr::bind_rows(
    tibble::tibble(vals = m_vals, sex = "male")
  ) |>
  dplyr::mutate(vals = vals * 100)

split_wshop_22 <- split(wshop_22, wshop_22$sex)
split_mix_r2 <- split(mix_r2, mix_r2$strategy)

# estimate mode of mixture distributions
modeest::mlv(split_mix_r2$female$mix_vals, method = "hsm")
modeest::mlv(split_mix_r2$male$mix_vals, method = "hsm")

# females
title <- paste0(
  "Comparison with previous workshop results from October 2022"
)
subtitle <- paste0(
  "In October 2022, the QOI was estimated for 2035; ",
  "vertical lines are QOI baselines (2019 and 2021)"
)
caption <- paste0(
  "Note: The 2019 QOI baseline value was subsequently revised ",
  "upward from 46.7% to 51.6%."
)

p6 <- plot_compare(split_wshop_22, split_mix_r2, sex = "female")

ggplot2::ggsave(
  here::here("figures", "workshop_comp_prev_f.png"),
  p6,
  width = 220, height = 165, units = c("mm")
)

# males
title <- paste0(
  "Comparison with previous workshop results from October 2022"
)
subtitle <- paste0(
  "In October 2022, the QOI was estimated for 2035; ",
  "vertical lines are QOI baselines (2019 and 2021)"
)
caption <- paste0(
  "Note: The 2019 QOI baseline value was subsequently revised ",
  "upward from 52.6% to 56.6%."
)

p7 <- plot_compare(split_wshop_22, split_mix_r2, sex = "male")

ggplot2::ggsave(
  here::here("figures", "workshop_comp_prev_m.png"),
  p7,
  width = 220, height = 165, units = c("mm")
)

# interpolate path for QOI ----
df_grid <- make_grid(params_r2)
df_interp_params <- interp_params(df_grid, params_r2)
df_interp_simul <- simul_interp(df_interp_params, interp_n = 1e5)
df_interp_mix <- mix_interp(df_interp_simul)

# check
df_interp_mix |>
  dplyr::filter(year == 2045) |>
  tidyr::unnest(mix_vals) |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = mix_vals)) +
  ggplot2::facet_wrap(ggplot2::vars(strategy))

# plot mixtures
# Joy Divsion, unknown pleasures
p8 <- plot_ridge(df_interp_mix |> tidyr::unnest(mix_vals))

ggplot2::ggsave(
  here::here("figures", "workshop_ridge_plot.png"),
  p8,
  width = 220, height = 165, units = c("mm")
)

# fanplot
p9 <- plot_fan(df_interp_mix |> tidyr::unnest(mix_vals))

ggplot2::ggsave(
  here::here("figures", "workshop_fan_plot.png"),
  p9,
  width = 220, height = 165, units = c("mm")
)

# save mixtures ----
mix_out <- df_interp_mix |>
  dplyr::rename(sex = strategy) |>
  dplyr::mutate(sex = dplyr::if_else(sex == "female", "f", "m"))

# convert to Arrow Table — handles list columns
mix_out_tb <- arrow::Table$create(mix_out)

arrow::write_parquet(
  mix_out_tb,
  here::here("data", "mixtures.parquet")
)
