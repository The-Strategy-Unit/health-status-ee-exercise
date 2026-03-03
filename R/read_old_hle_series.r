# README
# Read series on historic health state life expectancy (hle)
# From 'Hitting the right targets', Health Foundation, 2022
# https://www.health.org.uk/reports-and-analysis/working-papers/hitting-the-right-targets # nolint: line_length_linter.
# data for 1981-2001
# https://webarchive.nationalarchives.gov.uk/ukgwa/20160105163802mp_/http:/www.ons.gov.uk/ons/rel/disability-and-health-measurement/health-expectancies-at-birth-and-age-65-in-the-united-kingdom/1981-2001/healthy-life-expectancy-at-birth-and-at-65-in-great-britain-and-england--1981-2001.xls # nolint: line_length_linter.
# "data_raw/HealthExp198101_tcm77-202849.xls" # nolint: commented_code_linter.
# data for 2009-11 only (vs. modeled estimate for 2000-02) - not used
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/changesinhealthylifeexpectancyhle # nolint: line_length_linter.


# read old hle series ----
read_old_hle_series <- function(path) {

  sheets <- readxl::excel_sheets(path) |>
    stringr::str_subset(pattern = "^General Health")

  # iterate over sheets
  # not strictly necessary as only want a single sheet
  dat <- purrr::map(sheets, \(x) {
    readxl::read_xls(path, x, skip = 4) |>
      dplyr::rename_with(\(x) letters[seq_along(x)]) |>
      dplyr::select(a, b, g, m) |>
      tidyr::fill(a, .direction = "down") |>
      dplyr::filter(
        a == "ENGLAND",
        !is.na(b)
      ) |>
      dplyr::rename(
        year = b,
        m = g,
        f = m
      ) |>
      dplyr::select(-a) |>
      # for consistency with other series
      dplyr::mutate(
        area_code = "E92000001",
        area_name = "England",
        .after = "year"
      ) |>
      tidyr::pivot_longer(
        cols = c(f, m),
        names_to = "sex",
        values_to = "hle"
      ) |>
      dplyr::mutate(hle = dplyr::if_else(hle == ". .", NA, hle)) |>
      dplyr::mutate(
        year = as.integer(year),
        hle = as.double(hle)
      ) |>
      dplyr::mutate(age = 65L, .after = "sex")
  }) |>
    # combine sheets
    dplyr::bind_rows()
}
