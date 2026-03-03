# README
# Plotting functions to summarise workshop results


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

# plot comparison with previous exercise ----
plot_compare <- function(df1, df2, sex = "female") {

  col_sex <- dplyr::if_else(sex == {{ sex }}, pal_sex[1], pal_sex[2])

  ggplot2::ggplot() +
    ggplot2::geom_density(
      ggplot2::aes(x = vals),
      color = "#d3d3d3",
      data = df1[[sex]]
    ) +
    ggplot2::geom_density(
      ggplot2::aes(x = mix_vals),
      color = col_sex,
      data = df2[[sex]]
    ) +
    ggplot2::geom_vline(
      xintercept = qoi_2019_old[[sex]],
      linewidth = .5,
      linetype = "2222",
      color = "#d3d3d3"
    ) +
    ggplot2::geom_vline(
      xintercept = qoi_2019_new[[sex]],
      linewidth = .5,
      linetype = "1414",
      color = "#d3d3d3",
    ) +
    ggplot2::geom_vline(
      xintercept = qoi_2021[[sex]],
      linewidth = .5,
      linetype = "2222",
      color = col_sex,
    ) +
    ggplot2::scale_x_continuous(
      name = NULL,
      limits = c(25, NA),
      breaks = seq(30, 100, by = 10)
    ) +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    )
}

# plot ridges ----
plot_ridge <- function(df) {
  df |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = mix_vals,
        # y = reorder(year, desc(year)), # nolint: commented_code_linter.
        y = year,
        group = year,
        color = strategy
      )
    ) +
    geom_density_ridges(
      panel_scaling = FALSE,
      scale = 5,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::scale_y_discrete(name = NULL) +
    ggplot2::scale_color_manual(values = pal_sex) +
    ggplot2::facet_wrap(ggplot2::vars(strategy))
}

# plot fan ----
plot_fan <- function(df) {
  df |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = mix_vals)) +
    ggfan::geom_fan(show.legend = FALSE) +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::facet_wrap(ggplot2::vars(strategy))
}
