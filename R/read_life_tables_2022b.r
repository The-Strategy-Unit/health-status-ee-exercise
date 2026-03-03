# README
# Read 2022-based life tables
# x2 variant life tables were published alongside the principal

# fn to read life tables ----
prep_22_life_tbl <- function(path) {

  id <- stringr::str_extract(path, "[a-z]{3}(?=22ex)")

  sheets <- readxl::excel_sheets(path) |>
    stringr::str_subset(pattern = "period|cohort")

  # iterate over sheets
  purrr::map(sheets, \(x) {
    readxl::read_xlsx(path, x, skip = 3) |>
      dplyr::filter(!dplyr::row_number() == 1L) |>
      dplyr::rename_with(.cols = 1, ~ "age") |>
      dplyr::mutate(base = "2022b") |>
      tidyr::pivot_longer(
        cols = 2:93,
        names_to = "year",
        values_to = "ex"
      ) |>
      dplyr::mutate(
        age = as.integer(age),
        sex = tolower(stringr::str_extract(x, "female|male")), # f|m
        sex = stringr::str_sub(sex, 1L, 1L),
        type = stringr::str_extract(x, "period|cohort"),
        year = as.integer(stringr::str_extract(year, "[0-9]{4}$")) # fudge
      )
  }) |>
    # combine sheets
    dplyr::bind_rows() |>
    dplyr::mutate(id = id) |>
    # order columns
    dplyr::select(base, type, id, sex, year, age, ex)
}

# fn to write to csv ----
life_tbl_22_csv <- function(df) {
  path <- here::here("data", "life_tables_2022b.csv") # 2022b
  readr::write_csv(df, path)
  path
}
