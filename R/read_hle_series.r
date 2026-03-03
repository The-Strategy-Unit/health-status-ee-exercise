# README
# Read series on health state life expectancy (hle)
# Released: 12 December 2024
# "data_raw/healthylifeexpectancyenglandandwales.xlsx" # nolint: commented
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyallagesuk # nolint: line_length_linter.


# read hle series ----
read_hle_series <- function(path) {

  sheets <- readxl::excel_sheets(path) |>
    stringr::str_subset(pattern = "^1$")

  # iterate over sheets
  # not strictly necessary as only want a single sheet
  dat <- purrr::map(sheets, \(x) {
    readxl::read_xlsx(path, x, skip = 5) |>
      dplyr::rename_with(\(x) gsub(" ", "_", tolower(x))) |>
      dplyr::filter(country == "England") |>
      dplyr::mutate(
        sex = tolower(stringr::str_sub(sex, 1L, 1L)),
        start_year = stringr::str_extract(period, "^[0-9]{4}"),
        end_year = stringr::str_extract(period, "[0-9]{4}$"),
        dplyr::across(tidyselect::ends_with("year"), as.integer),
        age_group = stringr::str_replace(
          age_group,
          "[[:space:]]to[[:space:]]", "-"
        ),
        age_group = dplyr::case_match(
          age_group,
          "<1" ~ "under 1",
          .default = age_group
        )
      ) |>
      dplyr::rename_with(\(x) {
        purrr::map_chr(x, \(y) {
          stringr::str_c(
            stringr::str_sub(stringr::str_split(y, "_")[[1]], 1, 1)
            , collapse = ""
          )
        })
      }, tidyselect::ends_with("interval")
      | tidyselect::ends_with("expectancy")) |>
      dplyr::select(-country, -area_type, -sex_code, -age_band) |>
      dplyr::relocate(start_year, end_year, .after = "period") |>
      # warning relies on column ordering
      dplyr::rename("pc_life_good" = dplyr::last_col())
  }) |>
    # combine sheets
    dplyr::bind_rows()

  # limit to England
  dat <- dat |> dplyr::filter(area_code == "E92000001")
}
