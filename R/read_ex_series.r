# README
# Read series on period expectation of life (ex), by single year of age
# Released: 18 March 2025
# "data_raw/timeseries3yrex19802021.xlsx" # nolint: commented_code_linter.
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/periodexpectationoflifeexbysingleyearofage # nolint: line_length_linter.


# read ex series ----
read_ex_series <- function(path) {

  sheets <- readxl::excel_sheets(path) |>
    stringr::str_subset(pattern = "^Eng")

  # iterate over sheets
  purrr::map(sheets, \(x) {
    readxl::read_xlsx(path, x, skip = 4) |>
      tidyr::pivot_longer(
        cols = 2:43,
        names_to = "period",
        values_to = "ex"
      ) |>
      dplyr::mutate(
        sex = tolower(stringr::str_extract(x, "Females|Males")),
        sex = stringr::str_sub(sex, 1L, 1L),
        age = as.integer(age),
        start_year = stringr::str_extract(period, "^[0-9]{4}"),
        end_year = stringr::str_extract(period, "[0-9]{4}$"),
        dplyr::across(tidyselect::ends_with("year"), as.integer)
      ) |>
      dplyr::relocate(start_year, end_year, .after = "period") |>
      # for consistency with other series
      dplyr::mutate(
        area_code = "E92000001",
        area_name = "England",
        .after = "end_year"
      ) |>
      dplyr::relocate(sex, age, .after = "area_name")
  }) |>
    # combine sheets
    dplyr::bind_rows()
}
