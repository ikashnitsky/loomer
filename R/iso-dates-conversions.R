# format dates to iso ----------------------------------------------------------

#' Convert Date and Datetime Columns to ISO 8601 Strings
#'
#' @description
#' Detects date-like columns (`Date` and `POSIXct`) in a data frame and converts
#' them to formatted character strings formatted as `"YYYY-MM-DD"`.
#'
#' @param df A data frame, tibble, or `data.table`.
#'
#' @return An object of the same class as `df` with all `Date` and `POSIXct`
#'   columns replaced by character representations formatted as `YYYY-MM-DD`.
#'
#' @family date transformations
#' @export
#'
#' @importFrom dplyr mutate across where
#' @importFrom clock date_format
#'
#' @examples
#' sample_tbl <- tibble::tibble(
#'   event_id = 1:2,
#'   record_date = as.Date(c("2026-01-15", "2026-07-23")),
#'   record_time = as.POSIXct(c("2026-01-15 10:00:00", "2026-07-23 18:30:00"))
#' )
#'
#' to_yyyymmdd_iso_txt(sample_tbl)
to_yyyymmdd_iso_txt <- function(df) {
  dplyr::mutate(
    df,
    dplyr::across(
      dplyr::where(\(x) inherits(x, c("POSIXct", "Date"))),
      \(x) clock::date_format(x, format = "%Y-%m-%d")
    )
  )
}


# parse iso text to date -------------------------------------------------------

#' Parse ISO 8601 Character Columns to Date Objects
#'
#' @description
#' Parses character columns formatted as `"YYYY-MM-DD"` into proper `Date`
#' vectors using fast parsing via \pkg{fastymd}. If target columns are not
#' explicitly provided, candidate columns are inferred from the first row
#' element: only `character` columns whose first value matches the
#' `"YYYY-MM-DD"` pattern are converted. Factors, numeric and other columns are
#' never auto-detected.
#'
#' @param df A data frame or tibble.
#' @param date_cols An optional character vector of column names to convert. If
#'   `NULL` (the default), `character` columns whose first element matches
#'   `"YYYY-MM-DD"` are selected automatically.
#'
#' @return An object of the same class as `df` with selected character columns
#'   converted to `Date` objects.
#'
#' @family date transformations
#' @export
#'
#' @importFrom dplyr mutate across select where all_of
#'
#' @examples
#' sample_tbl <- tibble::tibble(
#'   id = 101:102,
#'   date_txt = c("2026-01-01", "2026-12-31"),
#'   code = c("AB", "CD")
#' )
#'
#' from_yyyymmdd_iso_txt(sample_tbl)
from_yyyymmdd_iso_txt <- function(df, date_cols = NULL) {
  if (is.null(date_cols)) {
    date_cols <- df |>
      dplyr::select(dplyr::where(\(x) {
        is.character(x) && length(x) > 0L && !is.na(x[1L]) &&
          grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x[1L])
      })) |>
      names()
  }

  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(date_cols),
        \(x) fastymd::fymd(x)
      )
    )
}

