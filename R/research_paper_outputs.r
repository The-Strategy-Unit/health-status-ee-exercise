# README
# Figures and tables to share with MM for research paper:
# 'Long term probabilistic forecasts of disability free life expectancy for
# males and females aged 65 years in England: an elicitation exercise'

# reproducibility:
# spnorm_simul() and spnorm_mix() both call rspnorm_trunc(),
# which uses fanplot::rsplitnorm() i.e., randomly generated
# with n = 1e6 I don't think this is of any material significance

# helpers
source(here::here("R", "read_ex_series.r"))
source(here::here("R", "read_proj_ex_series.r"))
source(here::here("R", "read_long_run_series.r"))
source(here::here("R", "read_dfle_series.r"))
source(here::here("R", "process_expert_params.r"))
source(here::here("R", "research_paper_plot_fns.r"))
source(here::here("R", "theme_hsee_exercise.r"))
source(here::here("R", "font_hoist.r"))
font_hoist("Fira Sans")
theme_hseee <- theme_hseee()
ggplot2::theme_set(theme_hseee)

# palettes
# okabeito_colors() # nolint: commented_code_linter.
# https://easystats.github.io/see/reference/scale_color_okabeito.html
pal_params <- c("#56B4E9", "#F5C710", "#56B4E9")
pal_sex <- c("#D55E00", "#0072B2")


# read timeseries ----
ex_series <- read_ex_series(
  here::here(
    "data_raw",
    "timeseries3yrex19802021.xlsx"
  )
)

proj_ex_series_files <- fs::dir_ls(
  path = here::here("data_raw"),
  regexp = "Figure_4.*csv$"
)

proj_ex_series <- purrr::map(
  proj_ex_series_files, \(x) read_proj_ex_series(x)
) |>
  dplyr::bind_rows() |>
  dplyr::filter(year > 2022L)

dfle_long <- read_long_dfle_series(
  here::here(
    "data_raw",
    "longrun_ts_dfle_65.csv"
  )
)

dfle_short <- read_dfle_series(
  here::here(
    "data_raw",
    "healthstatelifeexpectancyengwalni.xlsx"
  )
) |>
  dplyr::mutate(period = stringr::str_replace(period, " to ", "-"))

# prepare timeseries ----
ex_series <- ex_series |>
  dplyr::filter(age == 65L) |>
  dplyr::mutate(year = start_year + 1L) |>
  dplyr::select(year, sex, ex)

proj_ex_series <- proj_ex_series |>
  dplyr::select(year, sex, ex)

dfle_long <- dfle_long |>
  dplyr::filter(end_year <= 2011L) |>
  dplyr::select(start_year, sex, dfle)

dfle_short <- dfle_short |>
  dplyr::filter(age_group == "65-69") |>
  dplyr::select(start_year, sex, dfle)

dfle_series <- dplyr::bind_rows(dfle_long, dfle_short) |>
  dplyr::mutate(year = start_year + 1L) |>
  dplyr::select(-start_year)

ex_series_df <- dplyr::bind_rows(ex_series, proj_ex_series) |>
  dplyr::left_join(dfle_series, dplyr::join_by(year, sex)) |>
  dplyr::mutate(
    sex = dplyr::if_else(sex == "f", "Females", "Males")
  ) |>
  tidyr::pivot_longer(
    cols = c("ex", "dfle"),
    names_to = "measure",
    values_to = "value"
  ) |>
  dplyr::mutate(
    series = dplyr::case_when(
      year > 2022 & measure == "ex" ~ "ex-projection",
      measure == "ex" ~ "ex-historic",
      measure == "dfle" ~ "dfle-historic",
      TRUE ~ NA_character_
    )
  )

qoi_series_df <- ex_series_df |>
  dplyr::filter(year <= 2022L) |>
  tidyr::pivot_wider(
    id_cols = c(year, sex),
    names_from = measure
  ) |>
  dplyr::mutate(qoi = dfle / ex * 100)

# figures timeseries ----
ts1 <- plot_ex_series(ex_series_df)
ts2 <- plot_qoi_series(qoi_series_df)
ts3 <- patchwork::wrap_plots(
  ts1,
  patchwork::plot_spacer(),
  ts2,
  ncol = 1
) +
  patchwork::plot_layout(heights = c(1, 0.06, 1))

# save timeseries figures
ggsave(
  here::here("figures", "paper_ts1.png"),
  ts1,
  width = 220, height = 165, units = c("mm")
)

ggsave(
  here::here("figures", "paper_ts2.png"),
  ts2,
  width = 220, height = 165, units = c("mm")
)

ggsave(
  here::here("figures", "paper_ts3.png"),
  ts3,
  width = 220, height = 330, units = c("mm")
)

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

# table-1 ----
t1 <- dplyr::bind_rows(res_r1, res_r2) |>
  dplyr::select(id, strategy, which_phase, lo, mode, hi) |>
  dplyr::rename(
    p10 = lo, p90 = hi, sex = strategy, round = which_phase
  ) |>
  dplyr::mutate(
    round = paste0("r", round),
    sex = dplyr::if_else(sex == "female", "f", "m"),
    dplyr::across(where(is.double), as.integer)
  ) |>
  tidyr::pivot_wider(
    id_cols = id,
    names_from = c(sex, round),
    values_from = c(p10, mode, p90),
    names_vary = "slowest"
  ) |>
  dplyr::relocate(p10_f_r2:p90_f_r2, .before = p10_m_r1)

# get mixture
params_r2 <- get_params(res_r2)
simul_r2 <- get_simul(params_r2, simul_n = 1e6)
mix_r2 <- get_mix(simul_r2, mix_n = 1e6)

split_mix_r2 <- split(mix_r2, mix_r2$strategy)

# summarise mixture
pooled_p10_f_r2 <- quantile(split_mix_r2$female$mix_vals, probs = 0.1)
pooled_mode_f_r2 <- modeest::mlv(split_mix_r2$female$mix_vals, method = "hsm")
pooled_p90_f_r2 <- quantile(split_mix_r2$female$mix_vals, probs = 0.9)
pooled_p10_m_r2 <- quantile(split_mix_r2$male$mix_vals, probs = 0.1)
pooled_mode_m_r2 <- modeest::mlv(split_mix_r2$male$mix_vals, method = "hsm")
pooled_p90_m_r2 <- quantile(split_mix_r2$male$mix_vals, probs = 0.9)

# update table-1
t1_last_row <- tibble::tibble(
  id = "Pooled",
  p10_f_r1 = NA_integer_,
  mode_f_r1 = NA_integer_,
  p90_f_r1 = NA_integer_,
  p10_f_r2 = pooled_p10_f_r2,
  mode_f_r2 = pooled_mode_f_r2,
  p90_f_r2 = pooled_p90_f_r2,
  p10_m_r1 = NA_integer_,
  mode_m_r1 = NA_integer_,
  p90_m_r1 = NA_integer_,
  p10_m_r2 = pooled_p10_m_r2,
  mode_m_r2 = pooled_mode_m_r2,
  p90_m_r2 = pooled_p90_m_r2
)

t1_out <- dplyr::bind_rows(t1, t1_last_row)

# save table-1
readr::write_csv(t1_out, here::here("data", "paper_table-1.csv"))

# figures params ----
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
  here::here("figures", "paper_r1_params.png"),
  p1,
  width = 220, height = 165, units = c("mm")
)

ggsave(
  here::here("figures", "paper_r2_params.png"),
  p2,
  width = 220, height = 165, units = c("mm")
)

# figures pdfs ----
params_r1 <- get_params(res_r1)
params_r2 <- get_params(res_r2)

simul_r1 <- get_simul(params_r1, simul_n = 1e6)
simul_r2 <- get_simul(params_r2, simul_n = 1e6)

p3 <- plot_pdfs(simul_r1)
p4 <- plot_pdfs(simul_r2)

ggsave(
  here::here("figures", "paper_r1_pdfs.png"),
  p3,
  width = 220, height = 165, units = c("mm")
)

ggsave(
  here::here("figures", "paper_r2_pdfs.png"),
  p4,
  width = 220, height = 165, units = c("mm")
)

# figure mixture ----
p5 <- plot_mix(mix_r2, simul_r2)

ggsave(
  here::here("figures", "paper_r2_mixture.png"),
  p5,
  width = 220, height = 165, units = c("mm")
)

# figure mixture - for GitHub README ----
facet_labels <- function(x) {
  data.frame(
    label = stringr::str_to_title(x$strategy)
  )
}

title <- paste0(
  "Experts' Combined Distribution for the Proportion of Remaining Life Expectancy Spent Disability-Free\n at Age 65 in 2045"
)
subtitle <- paste0(
  "Distributions were combined using equal-weight linear pooling"
)

p6 <- mix_r2 |>
  ggplot2::ggplot() +
  ggplot2::geom_density(
    ggplot2::aes(x = mix_vals, color = strategy),
    show.legend = FALSE,
    linewidth = 1,
  ) +
  ggplot2::scale_color_manual(values = pal_sex) +
  ggplot2::scale_x_continuous(
    name = NULL,
    breaks = seq(0, 100, by = 20),
    limits = c(min_p10, max_p90)
  ) +
  ggplot2::scale_y_continuous(name = NULL) +
  ggplot2::facet_wrap(
    ggplot2::vars(which_phase, strategy),
    labeller = facet_labels
  ) +
  labs(
    title = title,
    subtitle = subtitle
  ) +
  ggplot2::theme(
    panel.background = element_rect(color = NA, fill = "#fff1e6"),
    plot.background = element_rect(color = NA, fill = "#fff1e6")
  )

ggsave(
  here::here("figures", "github_readme.png"),
  p6,
  width = 220, height = 165, units = c("mm")
)
