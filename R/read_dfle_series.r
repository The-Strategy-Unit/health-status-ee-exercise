# README
# Read series on health state life expectancy (dfle)
# This is the most recent version, at 2025-09-03, to include data on DFLE,
# subsequent releases only include HLE. From ONS, "Production of DFLE statistics
# has been paused until we can review the potential impact of the change to the
# disability census question between census 2011 and 2021."
# Released: 26 March 2024
# "data_raw/healthstatelifeexpectancyengwalni.xlsx" # nolint: commented_code_linter, line_length_linter.
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyallagesuk/current # nolint: line_length_linter.


# read dfles series ----
read_dfle_series <- function(path) {

  sheets <- readxl::excel_sheets(path) |>
    stringr::str_subset(pattern = "^3$")

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
      }, tidyselect::ends_with("interval")) |>
      dplyr::select(-country, -area_type, -sex_code, -age_band) |>
      dplyr::relocate(start_year, end_year, .after = "period") |>
      dplyr::rename_with(\(x) "dfle", .cols = ends_with("(DFLE)")) |>
      # warning relies on column ordering
      dplyr::rename("pc_life_free" = dplyr::last_col())
  }) |>
    # combine sheets
    dplyr::bind_rows()

  # limit to England
  dat <- dat |> dplyr::filter(area_code == "E92000001")
}
