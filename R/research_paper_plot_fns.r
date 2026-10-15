# README
# Plotting functions to create figures to share with MM for research paper:
# 'Long term probabilistic forecasts of disability free life expectancy for
# males and females aged 65 years in England: an elicitation exercise'

# shapes scale
shp_proj <- c(
  "ex-historic" = 16,
  "ex-projection" = 1,
  "dfle-historic" = 16
) # [ex-historic|ex-projection|dfle-historic]

# palettes
# okabeito_colors() # nolint: commented_code_linter.
# https://easystats.github.io/see/reference/scale_color_okabeito.html
pal_params <- c("#56B4E9", "#F5C710", "#56B4E9")
pal_measure <- c("#E69F00", "#56B4E9")


# plot LE and DFLE series ----
# Historic (1980-1982 to 2021-23) and projected (2023 to 2047)
# LE and DFLE (1980-82 to 2020-22) at age 65, England
plot_ex_series <- function(df) {

  df |>
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(
        x = year,
        y = value,
        group = measure,
        shape = series,
        color = measure
      ),
      size = 1.4
    ) +
    ggplot2::scale_x_continuous(
      name = "Year",
      limits = c(1980, 2048),
      expand = ggplot2::expansion(add = c(0, 0)),
      breaks = seq.int(1980, 2040, by = 10)
    ) +
    ggplot2::scale_y_continuous(
      name = "Life expectancy and DFLE at age 65 (years)",
      limits = c(6, 24),
      expand = ggplot2::expansion(add = c(.5, .5)),
      breaks = seq.int(6, 24, by = 4),
      # base R trims trailing spaces when drawing text
      # and regular space does not occupy same width as percent symbol
      labels = function(x) paste0(x, "<span style='color:transparent'>.</span>") # nolint: line_length_linter.
    ) +
    ggplot2::scale_shape_manual(values = shp_proj, guide = "none") +
    ggplot2::scale_color_manual(
      values = pal_measure,
      breaks = c("ex", "dfle"),
      labels = c("Life expectancy", "DFLE"),
      guide = ggplot2::guide_legend(
        title = NULL,
        override.aes = list(
          shape = 16,
          size = 2.5
        )
      )
    ) +
    ggplot2::facet_wrap(ggplot2::vars(sex)) +
    ggplot2::theme(
      axis.text.y = ggtext::element_markdown(
        family = "Fira Sans tnum",
        size = 11,
        hjust = 1
      ),
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.03),
      legend.justification.inside = c(1, 0),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        family = "Fira Sans",
        size = 11
      )
    )
}

# plot QOI series ----
# Percentage of remaining life free from disability at age 65, England,
# 1980-82 to 2020-22
plot_qoi_series <- function(df) {

  df |>
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(
        x = year,
        y = qoi
      ),
      color = "#009E73",
      size = 1.4,
      shape = 16,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = year,
        y = qoi
      ),
      color = "#009E73",
      linewidth = .5,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      name = "Year",
      limits = c(1980, 2048),
      expand = ggplot2::expansion(add = c(0, 0)),
      breaks = seq.int(1980, 2040, by = 10)
    ) +
    ggplot2::scale_y_continuous(
      name = "Remaining life disability-free at age 65 (%)",
      limits = c(45, 62),
      expand = expansion(add = c(.5, .5)),
      breaks = seq.int(40, 65, by = 5),
      # labels = function(x) paste0(x, "%")
      labels = function(x) paste0(x, "<span style='color:transparent'>.</span>") # nolint: line_length_linter.
    ) +
    ggplot2::facet_wrap(ggplot2::vars(sex)) +
    ggplot2::theme(
      axis.text.y = ggtext::element_markdown(
        family = "Fira Sans tnum",
        size = 10,
        hjust = 1
      )
    )
}

# plot params ----
facet_labels <- function(x) {
  data.frame(
    label = paste(
      x$which_phase,
      stringr::str_to_title(x$strategy),
      sep = ": "
    )
  )
}

plot_params <- function(df) {

  title <- paste0(
    "Expert parameter estimates, round-",
    df$which_phase[1]
  )
  subtitle <- paste0(
    "Each point represents an expert's estimate of the ",
    "10th percentile, mode, and 90th percentile"
  )

  df |>
    ggplot2::ggplot() +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = value,
        y = id,
        color = param
      ),
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = pal_params) +
    ggplot2::scale_x_continuous(
      name = NULL,
      breaks = seq(0, 100, by = 20),
      limits = c(0, 100)
    ) +
    ggplot2::scale_y_discrete(name = NULL) +
    ggplot2::facet_wrap(
      ggplot2::vars(which_phase, strategy),
      labeller = facet_labels
    ) +
    labs(
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme(
      panel.grid.major.x = element_line(
        linewidth = .2,
        linetype = "2222",
        color = "#d3d3d3"
      ),
    )
}

# plot simulated pdfs ----
plot_pdfs <- function(df) {

  title <- paste0(
    "Expert distributions, round-",
    df$which_phase[1]
  )
  subtitle <- paste0(
    "Each distribution encodes a different expert's beliefs ",
    "about the QOI in 2045"
  )

  df |>
    tidyr::unnest_longer(sim_vals) |>
    ggplot2::ggplot() +
    ggplot2::geom_density(
      ggplot2::aes(x = sim_vals, color = email),
      show.legend = FALSE
    ) +
    # ggplot2::scale_color_manual(values = MetBrewer::met.brewer("Hiroshige")) +
    see::scale_color_okabeito() +
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
    )
}

# plot simulated mixture ----
plot_mix <- function(df_mix, df_simul) {

  title <- paste0(
    "Final combined view of QOI in 2045, round-",
    df_mix$which_phase[1]
  )
  subtitle <- paste0(
    "Distributions are combined using equal-weight linear pooling"
  )

  df_mix |>
    ggplot2::ggplot() +
    # individual expert densities
    ggplot2::geom_density(
      ggplot2::aes(x = sim_vals, group = email),
      color = "#d8d8d8",
      data = df_simul |>
        dplyr::select(email, strategy, sim_vals) |>
        tidyr::unnest_longer(sim_vals)
    ) +
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
    )
}