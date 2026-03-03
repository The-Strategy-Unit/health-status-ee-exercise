# README
# Initial plots for internal discussion
# p1: le at age 65, 1980-82 to 2021-23
# p2: dfle at age 65, 1980-82 to 2020-22
# p3: dfle & hle at age 65, 2011-13 to 2021-23
# p4: historic and projected le at age 65, 1981 to 2047
# p5: hle at age 65, 1981 to 2001 & 2011-13 to 2021-23
# p6: dfle and hle at age 65 as % of le, 2011-13 to 2021-23
# p7: dfle and hle at age 65 as % of le, 1980-82 to 2021-23

# packages
library("dplyr")
library("fs")
library("ggplot2")
library("ggtext")
library("glue")
library("here")
library("MetBrewer")
library("patchwork")
library("ragg")
library("readr")
library("readxl")
library("see")
library("stringr")
library("tidyr")

# helpers
source(here::here("R", "read_dfle_series.r"))
source(here::here("R", "read_ex_series.r"))
source(here::here("R", "read_hle_series.r"))
source(here::here("R", "read_long_run_series.r"))
source(here::here("R", "read_old_hle_series.r"))
source(here::here("R", "read_proj_ex_series.r"))
source(here::here("R", "theme_hsee_exercise.r"))
source(here::here("R", "font_hoist.r"))
font_hoist("Fira Sans")
theme_hseee <- theme_hseee()
ggplot2::theme_set(theme_hseee)

# palettes
# okabeito_colors() # nolint: commented_code_linter.
# https://easystats.github.io/see/reference/scale_color_okabeito.html
pal_sex <- c("#D55E00", "#0072B2") # [f|m]
pal_measure <- c("#009E73",  "#CC79A7") # [dfle|hle]


# read data ----
ex <- read_ex_series(
  here(
    "data_raw",
    "timeseries3yrex19802021.xlsx"
  )
)

hle <- read_hle_series(
  here(
    "data_raw",
    "healthylifeexpectancyenglandandwales.xlsx"
  )
) |>
  mutate(period = str_replace(period, " to ", "-"))

dfle <- read_dfle_series(
  here(
    "data_raw",
    "healthstatelifeexpectancyengwalni.xlsx"
  )
) |>
  mutate(period = str_replace(period, " to ", "-"))

long_ex <- read_long_ex_series(
  here(
    "data_raw",
    "longrun_ts_le_65.csv"
  )
)

long_dfle <- read_long_dfle_series(
  here(
    "data_raw",
    "longrun_ts_dfle_65.csv"
  )
)

old_hle <- read_old_hle_series(
  here(
    "data_raw",
    "HealthExp198101_tcm77-202849.xls"
  )
)

proj_ex_files <- dir_ls(path = here("data_raw"), regexp = "Figure_4.*csv$")

proj_ex <- map(proj_ex_files, \(x) read_proj_ex_series(x))
proj_ex <- bind_rows(proj_ex)

# plot-1 ----
# le at age 65, 1980-82 to 2021-23
p1_subtitle <- "<span style='font-family:Fira Sans; color:#D55E00;'>Females</span><span style='font-family:Fira Sans;'> | </span><span style='font-family:Fira Sans; color:#0072B2;'>Males</span>" # nolint: line_length_linter.

p1_title <- "Life expectancy at age 65, England, 1980-82 to 2021-23"

p1 <- ex |>
  dplyr::filter(age == 65L) |>
  ggplot() +
  geom_point(
    aes(x = end_year, y = ex, group = sex, color = sex, fill = sex),
    shape = 21,
    size = 1.4,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    name = NULL,
    expand = expansion(add = c(2, 2))
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(12, 22),
    expand = expansion(add = c(.5, .5)),
    breaks = seq.int(12, 22, by = 2)
  ) +
  scale_color_manual(values = pal_sex) +
  scale_fill_manual(values = pal_sex) +
  labs(
    title = p1_title,
    subtitle = p1_subtitle
  ) +
  theme(
    plot.title = element_markdown(
      color = "#1d1d1d",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 12,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    ),
    plot.subtitle = element_markdown(
      color = "#686f73",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 10,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    )
  )

# save plot
ggsave(
  here("figures", "p1.png"),
  p1,
  width = 220, height = 165, units = c("mm")
)

# plot-2 ----
# dfle at age 65, 1980-82 to 2020-22
# data for plotting
pre_2011 <- long_dfle |> filter(end_year <= 2011L)
post_2011_old <- long_dfle |> filter(end_year > 2011L)
post_2011_new <- dfle |> filter(age_group == "65-69")

p2_subtitle <- "<span style='font-family:Fira Sans; color:#D55E00;'>Females</span><span style='font-family:Fira Sans;'> | </span><span style='font-family:Fira Sans; color:#0072B2;'>Males</span>" # nolint: line_length_linter.

p2_title <- "Disability free life expectancy at age 65, England, 1980-82 to 2020-22" # nolint: line_length_linter.

p2 <- ggplot() +
  geom_point(
    aes(x = end_year, y = dfle, group = sex, color = sex, fill = sex),
    shape = 21,
    size = 1.4,
    show.legend = FALSE,
    data = pre_2011 # includes some NA (missing) values
  ) +
  # geom_point(
  #   aes(x = end_year, y = dfle, group = sex, color = sex, fill = sex),
  #   shape = 24,
  #   alpha = 0.5,
  #   size = 1.4,
  #   show.legend = FALSE,
  #   data = post_2011_old
  # ) +
  geom_point(
    aes(x = end_year, y = dfle, group = sex, color = sex, fill = sex),
    shape = 21,
    size = 1.4,
    show.legend = FALSE,
    data = post_2011_new
  ) +
  scale_x_continuous(
    name = NULL,
    expand = expansion(add = c(2, 2))
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(6, 12),
    expand = expansion(add = c(.2, .2)),
    breaks = seq.int(6, 12, by = 1)
  ) +
  scale_color_manual(values = pal_sex) +
  scale_fill_manual(values = pal_sex) +
  labs(
    title = p2_title,
    subtitle = p2_subtitle
  ) +
  theme(
    plot.title = element_markdown(
      color = "#1d1d1d",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 12,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    ),
    plot.subtitle = element_markdown(
      color = "#686f73",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 10,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    )
  )

# save plot
ggsave(
  here("figures", "p2.png"),
  p2,
  width = 220, height = 165, units = c("mm")
)

# plot-3 ----
# dfle & hle at age 65, 2011-13 to 2021-23
# data for plotting
hle_65 <- hle |> dplyr::filter(age_group == "65-69")

p3_subtitle <- "<span style='font-family:Fira Sans; color:#009E73;'>DFLE</span><span style='font-family:Fira Sans;'> | </span><span style='font-family:Fira Sans; color:#CC79A7;'>HLE</span>" # nolint: line_length_linter.

p3_title <- "Disability free & healthy life expectancy at age 65, England, 2011-13 to 2021-23" # nolint: line_length_linter.

panel_labels <- as_labeller(
  c("f" = "Females", "m" = "Males")
)

p3 <- ggplot() +
  geom_point(
    aes(x = end_year, y = hle, group = 1L),
    shape = 21,
    color = "#CC79A7",
    fill = "#CC79A7",
    size = 1.4,
    show.legend = FALSE,
    data = hle_65
  ) +
  geom_point(
    aes(x = end_year, y = dfle, group = 1L),
    shape = 21,
    color = "#009E73",
    fill = "#009E73",
    size = 1.4,
    show.legend = FALSE,
    data = post_2011_new
  ) +
  geom_line(
    aes(x = end_year, y = dfle, group = 1L),
    linewidth = .6,
    linetype = "4242",
    color = "#009E73",
    show.legend = FALSE,
    data = post_2011_new
  ) +
  facet_wrap(vars(sex), labeller = panel_labels) +
  scale_x_continuous(
    name = NULL,
    expand = expansion(add = c(1, .8)),
    labels = c(seq.int(2012, 2022, by = 2), "")
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(9, 12),
    expand = expansion(add = c(.2, .2)),
    breaks = seq.int(9, 12, by = 1)
  ) +
  labs(
    title = p3_title,
    subtitle = p3_subtitle
  ) +
  theme(
    plot.title = element_markdown(
      color = "#1d1d1d",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 12,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    ),
    plot.subtitle = element_markdown(
      color = "#686f73",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 10,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    )
  )

# save plot
ggsave(
  here("figures", "p3.png"),
  p3,
  width = 220, height = 165, units = c("mm")
)

# plot-4 ----
# historic and projected le at age 65, 1981 to 2047
p4_subtitle <- "<span style='font-family:Fira Sans; color:#D55E00;'>Females</span><span style='font-family:Fira Sans;'> | </span><span style='font-family:Fira Sans; color:#0072B2;'>Males</span>" # nolint: line_length_linter.

p4_title <- "Historic and projected life expectancy at age 65, England, 1981 to 2047" # nolint: line_length_linter.

p4 <- ggplot() +
  geom_point(
    aes(x = year, y = ex, group = sex, color = sex, fill = sex),
    shape = 21,
    size = 1.4,
    show.legend = FALSE,
    data = proj_ex
  ) +
  scale_x_continuous(
    name = NULL,
    expand = expansion(add = c(2, 2)),
    breaks = seq.int(1980, 2040, by = 10)
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(12, 24),
    expand = expansion(add = c(.5, .5)),
    breaks = seq.int(12, 24, by = 2)
  ) +
  scale_color_manual(values = pal_sex) +
  scale_fill_manual(values = pal_sex) +
  labs(
    title = p4_title,
    subtitle = p4_subtitle
  ) +
  theme(
    plot.title = element_markdown(
      color = "#1d1d1d",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 12,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    ),
    plot.subtitle = element_markdown(
      color = "#686f73",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 10,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    )
  )

# save plot
ggsave(
  here("figures", "p4.png"),
  p4,
  width = 220, height = 165, units = c("mm")
)

# plot-5 ----
# hle at age 65, 1981 to 2001 & 2011-13 to 2021-23
p5_subtitle <- "<span style='font-family:Fira Sans; color:#D55E00;'>Females</span><span style='font-family:Fira Sans;'> | </span><span style='font-family:Fira Sans; color:#0072B2;'>Males</span>" # nolint: line_length_linter.

p5_title <- "Healthy life expectancy at age 65, England, 1981 to 2021-23" # nolint: line_length_linter.

p5 <- ggplot() +
  geom_point(
    aes(x = year, y = hle, group = sex, color = sex, fill = sex),
    shape = 21,
    size = 1.4,
    show.legend = FALSE,
    data = old_hle # includes some NA (missing) values
  ) +
  geom_point(
    aes(x = end_year, y = hle, group = sex, color = sex, fill = sex),
    shape = 21,
    size = 1.4,
    show.legend = FALSE,
    data = hle_65
  ) +
  scale_x_continuous(
    name = NULL,
    expand = expansion(add = c(2, 2))
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(8, 14),
    expand = expansion(add = c(.2, .2)),
    breaks = seq.int(8, 14, by = 1)
  ) +
  scale_color_manual(values = pal_sex) +
  scale_fill_manual(values = pal_sex) +
  labs(
    title = p5_title,
    subtitle = p5_subtitle
  ) +
  theme(
    plot.title = element_markdown(
      color = "#1d1d1d",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 12,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    ),
    plot.subtitle = element_markdown(
      color = "#686f73",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 10,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    )
  )

# save plot
ggsave(
  here("figures", "p5.png"),
  p5,
  width = 220, height = 165, units = c("mm")
)

# plot-6 ----
# dfle and hle at age 65 as % of le, 2011-13 to 2021-23
# data for plotting
p6_dat <- ex |>
  filter(start_year >= 2011L, age == 65L) |>
  left_join(
    post_2011_new |>
      select(-lci, -uci),
    join_by(
      period, start_year, end_year,
      area_code, area_name,
      sex
    )
  ) |>
  left_join(
    hle_65 |>
      select(-lci, -uci),
    join_by(
      period, start_year, end_year,
      area_code, area_name,
      sex
    )
  ) |>
  mutate(
    pc_life_free_2 = dfle / ex * 100,
    pc_life_good_2 = hle / ex * 100
  )

p6_subtitle <- "<span style='font-family:Fira Sans; color:#009E73;'>DFLE</span><span style='font-family:Fira Sans;'> | </span><span style='font-family:Fira Sans; color:#CC79A7;'>HLE</span>" # nolint: line_length_linter.

p6_title <- "Percentage of remaining life free from disability / in good health at age 65, England, 2011-13 to 2021-23" # nolint: line_length_linter.

panel_labels <- as_labeller(
  c("f" = "Females", "m" = "Males")
)

p6 <- ggplot() +
  geom_point(
    aes(x = end_year, y = pc_life_free_2, group = 1L),
    shape = 21,
    color = "#009E73",
    fill = "#009E73",
    size = 1.4,
    show.legend = FALSE,
    data = p6_dat # includes some NA (missing) values
  ) +
  geom_line(
    aes(x = end_year, y = pc_life_free_2, group = 1L),
    linewidth = .6,
    linetype = "4242",
    color = "#009E73",
    show.legend = FALSE,
    data = p6_dat
  ) +
  geom_point(
    aes(x = end_year, y = pc_life_good_2, group = 1L),
    shape = 24,
    color = "#CC79A7",
    fill = "#CC79A7",
    size = 1.4,
    show.legend = FALSE,
    data = p6_dat
  ) +
  facet_wrap(vars(sex), labeller = panel_labels) +
  scale_x_continuous(
    name = NULL,
    expand = expansion(add = c(1, .8)),
    labels = c(seq.int(2012, 2022, by = 2), "")
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(48, 58),
    expand = expansion(add = c(.5, .5)),
    breaks = seq.int(48, 58, by = 2)
  ) +
  scale_color_manual(values = pal_sex) +
  scale_fill_manual(values = pal_sex) +
  labs(
    title = p6_title,
    subtitle = p6_subtitle
  ) +
  theme(
    plot.title = element_markdown(
      color = "#1d1d1d",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 12,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    ),
    plot.subtitle = element_markdown(
      color = "#686f73",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 10,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    )
  )

# save plot
ggsave(
  here("figures", "p6.png"),
  p6,
  width = 220, height = 165, units = c("mm")
)

# plot-7 ----
# dfle and hle at age 65 as % of le, 1980-82 to 2021-23
# data for plotting
p7_dat <- ex |>
  filter(age == 65L) |>
  left_join(
    bind_rows(
      # dfle pre-2011
      pre_2011,
      # dfle 2014-16 onward *NEW*
      post_2011_new
    ) |>
      select(-age, -age_group, -lci, -uci),
    join_by(
      period, start_year, end_year,
      area_code, area_name,
      sex
    )
  ) |>
  left_join(
    bind_rows(
      # hle pre-2002
      old_hle |>
        mutate(end_year = year),
      # hle 2011-13 onward *NEW*
      hle_65
    ) |>
      select(
        -period, -start_year,
        -age, -age_group,
        -lci, -uci
      ),
    join_by(
      end_year,
      area_code, area_name,
      sex
    )
  ) |>
  mutate(
    pc_life_free_2 = dfle / ex * 100,
    pc_life_good_2 = hle / ex * 100
  )

p7_subtitle <- "<span style='font-family:Fira Sans; color:#009E73;'>DFLE</span><span style='font-family:Fira Sans;'> | </span><span style='font-family:Fira Sans; color:#CC79A7;'>HLE</span>" # nolint: line_length_linter.

p7_title <- "Percentage of remaining life free from disability / in good health at age 65, England, 1980-82 to 2021-23" # nolint: line_length_linter.

panel_labels <- as_labeller(
  c("f" = "Females", "m" = "Males")
)

p7 <- ggplot() +
  geom_point(
    aes(x = end_year, y = pc_life_free_2, group = 1L),
    shape = 21,
    color = "#009E73",
    fill = "#009E73",
    size = 1.4,
    show.legend = FALSE,
    data = p7_dat # includes some NA (missing) values
  ) +
  geom_line(
    aes(x = end_year, y = pc_life_free_2, group = 1L),
    linewidth = .6,
    linetype = "4242",
    color = "#009E73",
    show.legend = FALSE,
    data = p7_dat
  ) +
  geom_point(
    aes(x = end_year, y = pc_life_good_2, group = 1L),
    shape = 24,
    color = "#CC79A7",
    fill = "#CC79A7",
    size = 1.4,
    show.legend = FALSE,
    data = p7_dat
  ) +
  geom_line(
    aes(x = end_year, y = pc_life_good_2, group = 1L),
    linewidth = .6,
    linetype = "4242",
    color = "#CC79A7",
    show.legend = FALSE,
    data = p7_dat
  ) +
  facet_wrap(vars(sex), labeller = panel_labels) +
  scale_x_continuous(
    name = NULL,
    limits = c(1980, 2023),
    expand = expansion(add = c(1, .8)),
    breaks = seq.int(1980, 2020, by = 10)
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(45, 80),
    expand = expansion(add = c(.5, .5)),
    breaks = seq.int(40, 80, by = 10)
  ) +
  scale_color_manual(values = pal_sex) +
  scale_fill_manual(values = pal_sex) +
  labs(
    title = p7_title,
    subtitle = p7_subtitle
  ) +
  theme(
    plot.title = element_markdown(
      color = "#1d1d1d",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 12,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    ),
    plot.subtitle = element_markdown(
      color = "#686f73",
      family = "Fira Sans Medium",
      lineheight = 1.2,
      size = 10,
      margin = margin(b = 4 * 2),
      hjust = 0,
      vjust = 1
    )
  )

# save plot
ggsave(
  here("figures", "p7.png"),
  p7,
  width = 220, height = 165, units = c("mm")
)
