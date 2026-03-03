# README
# Read long-run time series on life expectancy (ex) and disability free life
# expectancy (dfle); supplied by Chris White at ONS.
# The estimates for 2014-16 to 2018-20  supplied by Chris match those in the
# file labelled date superseded 26 March 2024 [xlsx 32.2MB] from the link below.
# "data_raw/longrun_ts_dfle_65.csv" # nolint: commented_code_linter.
# "data_raw/longrun_ts_le_65.csv" # nolint: commented_code_linter.
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyallagesuk/current # nolint: line_length_linter.


# read long ex series ----
read_long_ex_series <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(period = paste(start, end, sep = "-")) |>
    tidyr::pivot_longer(
      cols = c("f", "m"),
      values_to = "ex",
      names_to = "sex"
    ) |>
    dplyr::rename_with(\(x) paste(x, "year", sep = "_"), c("start", "end")) |>
    # for consistency with other series
    dplyr::mutate(
      area_code = "E92000001",
      area_name = "England",
      .after = "end_year"
    ) |>
    dplyr::mutate(age = 65L, .after = "sex") |>
    dplyr::relocate(period)
}

# read long dfle series ----
read_long_dfle_series <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(period = paste(start, end, sep = "-")) |>
    tidyr::pivot_longer(
      cols = c("f", "m"),
      values_to = "dfle",
      names_to = "sex"
    ) |>
    dplyr::rename_with(\(x) paste(x, "year", sep = "_"), c("start", "end")) |>
    # for consistency with other series
    dplyr::mutate(
      area_code = "E92000001",
      area_name = "England",
      .after = "end_year"
    ) |>
    dplyr::mutate(age = 65L, .after = "sex") |>
    dplyr::relocate(period)
}
