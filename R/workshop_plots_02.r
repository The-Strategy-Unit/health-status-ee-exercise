# README
# Final plots for sharing in the workshop
# p1: historic and projected le at age 65, 1981 to 2047
# p2: dfle at age 65, 1980-82 to 2020-22
# p3: dfle and hle at age 65 as % of le, 1980-82 to 2021-23

# Useful information
# https://blog.ons.gov.uk/2024/12/12/a-new-way-to-measure-healthy-life-expectancy/ # nolint: line_length_linter.
# see section '5. Comparisons with previous projections of life expectancy'
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/disability/articles/disabilitybyagesexanddeprivationenglandandwales/census2021 # nolint: line_length_linter.
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/methodologies/healthstatelifeexpectanciesukqmi2020to2022 # nolint: line_length_linter.
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/methodologies/nationalpopulationprojectionsmortalityassumptions2022based # nolint: line_length_linter.

# packages
library("dplyr")
library("fs")
library("ggplot2")
library("ggtext")
library("glue")
library("here")
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
source(here::here("R", "read_long_run_series.r"))
source(here::here("R", "read_proj_ex_series.r"))
source(here::here("R", "theme_hsee_exercise.r"))
source(here::here("R", "font_hoist.r"))
font_hoist("Fira Sans")
theme_hseee <- theme_hseee()
ggplot2::theme_set(theme_hseee)

# palettes
pal_sex <- c("#D55E00", "#0072B2") # [f|m]
pal_grp <- c("#D55E00", "#E69F00", "#0072B2", "#56B4E9")
# [f-historic|f-proj|m-historic|m-proj]


# read data ----
ex <- read_ex_series(
  here(
    "data_raw",
    "timeseries3yrex19802021.xlsx"
  )
)

proj_ex_files <- dir_ls(path = here("data_raw"), regexp = "Figure_4.*csv$")

proj_ex <- map(proj_ex_files, \(x) read_proj_ex_series(x))
proj_ex <- bind_rows(proj_ex) |>
  mutate(
    proj = if_else(year > 2022, "projection", "historic"),
    plot_grp = paste(sex, proj, sep = "-")
  )

dfle <- read_dfle_series(
  here(
    "data_raw",
    "healthstatelifeexpectancyengwalni.xlsx"
  )
) |>
  mutate(period = str_replace(period, " to ", "-"))

long_dfle <- read_long_dfle_series(
  here(
    "data_raw",
    "longrun_ts_dfle_65.csv"
  )
)

# plot-1 ----
# historic and projected le at age 65, 1981 to 2047
p1_subtitle <- "<span style='font-family:Fira Sans; color:#D55E00;'>Females</span><span style='font-family:Fira Sans;'> | </span><span style='font-family:Fira Sans; color:#0072B2;'>Males</span>" # nolint: line_length_linter.

p1_title <- "Historic and projected life expectancy at age 65, England, 1981 to 2047" # nolint: line_length_linter.

p1_caption <- "Source: ONS National Population Projections 2022-based, mortality assumptions." # nolint: line_length_linter.

p1 <- ggplot() +
  geom_point(
    aes(x = year, y = ex, group = plot_grp, color = plot_grp, fill = plot_grp),
    shape = 21,
    size = 1.4,
    show.legend = FALSE,
    data = proj_ex
  ) +
  scale_x_continuous(
    name = NULL,
    expand = expansion(add = c(2, 4)),
    breaks = seq.int(1980, 2050, by = 10)
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(12, 24),
    expand = expansion(add = c(.5, .5)),
    breaks = seq.int(12, 24, by = 2)
  ) +
  scale_color_manual(values = pal_grp) +
  scale_fill_manual(values = pal_grp) +
  labs(
    title = p1_title,
    subtitle = p1_subtitle,
    caption = p1_caption
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
  here("figures", "workshop_p1.png"),
  p1,
  width = 220, height = 165, units = c("mm")
)

# plot-2 ----
# dfle at age 65, 1980-82 to 2020-22
pre_2011 <- long_dfle |> filter(end_year <= 2011L)
post_2011_old <- long_dfle |> filter(end_year > 2011L)
post_2011_new <- dfle |> filter(age_group == "65-69")

p2_subtitle <- "<span style='font-family:Fira Sans; color:#D55E00;'>Females</span><span style='font-family:Fira Sans;'> | </span><span style='font-family:Fira Sans; color:#0072B2;'>Males</span>" # nolint: line_length_linter.

p2_title <- "Disability free life expectancy at age 65, England, 1980-82 to 2020-22" # nolint: line_length_linter.

p2_caption <- "Source: ONS Health State Life Expectancy series." # nolint: line_length_linter.

p2 <- ggplot() +
  geom_point(
    aes(x = end_year, y = dfle, group = sex, color = sex, fill = sex),
    shape = 21,
    size = 1.4,
    show.legend = FALSE,
    data = pre_2011 # includes some NA (missing) values
  ) +
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
    subtitle = p2_subtitle,
    caption = p2_caption
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
  here("figures", "workshop_p2.png"),
  p2,
  width = 220, height = 165, units = c("mm")
)

# plot-3 ----
# dfle and hle at age 65 as % of le, 1980-82 to 2021-23
p3_dat <- ex |>
  filter(
    age == 65L,
    # most recent dfle data is 2020-22
    period != "2021-2023"
  ) |>
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
  mutate(
    # years lived with disability
    dle = ex - dfle,
    pc_life_free_2 = dfle / ex * 100
  )

p3_title <- "Percentage of remaining life free from disability at age 65, England, 1980-82 to 2020-22" # nolint: line_length_linter.

p3_caption <- "Sources: ONS National Population Projections 2022-based, mortality assumptions; ONS Health State Life Expectancy series." # nolint: line_length_linter.

panel_labels <- as_labeller(
  c("f" = "Females", "m" = "Males")
)

p3 <- ggplot() +
  geom_point(
    aes(
      x = end_year,
      y = pc_life_free_2,
      group = 1L,
      color = factor(sex),
      fill = factor(sex)
    ),
    shape = 21,
    size = 1.4,
    show.legend = FALSE,
    data = p3_dat # includes some NA (missing) values
  ) +
  geom_line(
    aes(
      x = end_year,
      y = pc_life_free_2,
      group = 1L,
      color = factor(sex)
    ),
    linewidth = .5,
    show.legend = FALSE,
    data = p3_dat
  ) +
  facet_wrap(vars(sex), labeller = panel_labels) +
  scale_color_manual(values = pal_sex) +
  scale_fill_manual(values = pal_sex) +
  scale_x_continuous(
    name = NULL,
    limits = c(1980, 2023),
    expand = expansion(add = c(1, .8)),
    breaks = seq.int(1980, 2020, by = 10)
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(45, 62),
    expand = expansion(add = c(.5, .5)),
    breaks = seq.int(40, 65, by = 5),
    minor_breaks = seq.int(45, 62, by = 1),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = p3_title,
    caption = p3_caption
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
    ),
    panel.grid.minor.y = element_line(
      linewidth = .2,
      linetype = "4114",
      color = "#d3d3d3"
    )
  )

# save plot
ggsave(
  here("figures", "workshop_p3.png"),
  p3,
  width = 220, height = 165, units = c("mm")
)

# years lived with disability
p3_dat |>
  filter(sex == "m") |>
  select(period, sex, ex, dfle, dle, pc_life_free_2) |>
  print(n = 41)

ggplot() +
  geom_point(
    aes(
      x = end_year,
      y = dle,
      group = 1L,
      color = factor(sex),
      fill = factor(sex)
    ),
    shape = 21,
    size = 1.4,
    show.legend = FALSE,
    data = p3_dat # includes some NA (missing) values
  ) +
  facet_wrap(vars(sex), labeller = panel_labels) +
  scale_color_manual(values = pal_sex) +
  scale_fill_manual(values = pal_sex)
