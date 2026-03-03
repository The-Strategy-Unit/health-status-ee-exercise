# README
# Read series on projected period expectation of life (ex) at age 65
# Figs 4a and 4b from link below
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/methodologies/nationalpopulationprojectionsmortalityassumptions2022based # nolint: line_length_linter.
# "data_raw/Figure_4b_Female_life_expectancy_at_age_65_years_next_25_years.csv" # nolint: commented_code_linter, line_length_linter.
# "data_raw/Figure_4a_Male_life_expectancy_at_age_65_years_next_25_years.csv" # nolint: commented_code_linter, line_length_linter.


# read proj ex series ----
read_proj_ex_series <- function(path) {

  sex <- stringr::str_extract(path, "Female|Male")

  readr::read_csv(path, skip = 6, show_col_types = FALSE) |>
    dplyr::rename_with(\(x) tolower(x)) |>
    dplyr::select(year, england) |>
    dplyr::rename(ex = england) |>
    # for consistency with other series
    dplyr::mutate(
      area_code = "E92000001",
      area_name = "England",
      .after = "year"
    ) |>
    dplyr::mutate(sex = tolower(stringr::str_sub(sex, 1L, 1L))) |>
    dplyr::mutate(age = 65L, .after = "sex")
}
