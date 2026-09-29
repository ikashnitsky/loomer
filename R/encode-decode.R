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
#' Parses 10-character string columns formatted as `"YYYY-MM-DD"` into proper
#' `Date` vectors using fast parsing via \pkg{fastymd}. If target columns are not
#' explicitly provided, candidate columns are inferred by evaluating string length
#' on the first row element.
#'
#' @param df A data frame or tibble.
#' @param date_cols An optional character vector of column names to convert. If
#'   `NULL` (the default), columns where the first element is exactly 10 characters
#'   long are selected automatically.
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
        a <- x[1]
        !is.na(a) && nchar(a) == 10
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


# encode character columns -----------------------------------------------------

#' Encode Categorical Columns to Integers with Codebook Mapping
#'
#' @description
#' Detects categorical columns (`character` or `factor`), optionally filtering
#' by name patterns (e.g., matching `"START"` or `"FINAL"` and excluding `"DATE"`
#' or `"DAY"`). Extracts distinct levels while preserving factor order, maps
#' values to dense sequential integers via [collapse::fmatch()], and converts
#' values to character strings for safe database transmission (e.g., Oracle/ROracle).
#'
#' @param df A data frame, tibble, or `data.table`.
#' @param cols An optional tidyselect specification of columns to encode
#'   (e.g., `dplyr::matches("START|FINAL") & !dplyr::matches("DATE|DAY")`).
#'   If `NULL` (the default), automatically selects all `character` and `factor`
#'   columns matching `"START|FINAL"` and excluding `"DATE|DAY"`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{`encoded`}{A modified version of `df` with encoded columns replaced by character strings of their integer codes.}
#'   \item{`codebook`}{A `tibble` mapping column names (`VARIABLE`), original labels (`TEXT_LABEL`), and sequential codes (`INTEGER_CODE`).}
#' }
#'
#' @family encoding workflows
#' @export
#'
#' @importFrom collapse fmatch get_vars<-
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr
#' @importFrom tibble tibble
#'
#' @examples
#' test_df <- tibble::tibble(
#'   EVENT_START_STATUS = factor(c("Active", "Pending", "Active", "Archived"),
#'                               levels = c("Pending", "Active", "Archived")),
#'   EVENT_FINAL_REASON = c("Completed", "Dropout", "Completed", "Transferred"),
#'   START_DATE         = as.Date(c("2026-01-01", "2026-02-01", "2026-03-01", "2026-04-01")),
#'   VALUE_NUM          = c(10.5, 23.1, 14.2, 9.8)
#' )
#'
#' res <- encode_chars_to_int(test_df)
#' res$encoded
#' res$codebook
encode_chars_to_int <- function(df, cols = NULL) {
  # 1. Resolve target columns --------------------------------------------------
  is_cat <- vapply(df, function(x) is.character(x) || is.factor(x), logical(1))

  if (is.null(substitute(cols))) {
    # Default behavior: START/FINAL pattern excluding DATE/DAY
    target_names <- grep(
      pattern = "START|FINAL",
      x = names(df)[is_cat],
      value = TRUE,
      ignore.case = TRUE
    )
    target_names <- grep(
      pattern = "DATE|DAY",
      x = target_names,
      value = TRUE,
      invert = TRUE,
      ignore.case = TRUE
    )
  } else {
    # Custom tidyselect expression
    pos <- tidyselect::eval_select(
      rlang::enquo(cols),
      data = df
    )
    # Intersect with categorical-only columns
    target_names <- names(pos)[is_cat[pos]]
  }

  empty_codebook <- tibble::tibble(
    VARIABLE = character(),
    TEXT_LABEL = character(),
    INTEGER_CODE = integer()
  )

  if (length(target_names) == 0L) {
    return(list(encoded = df, codebook = empty_codebook))
  }

  # 2. Extract ordered levels per variable -------------------------------------
  levels_map <- lapply(df[target_names], function(x) {
    if (is.factor(x)) {
      # Retain original factor levels hierarchy
      levels(x)
    } else {
      # Use fast unique extraction, then sort alphabetically
      sort(kit::funique(x), na.last = TRUE)
    }
  })

  n_levels <- lengths(levels_map)

  codebook <- tibble::tibble(
    VARIABLE = rep(target_names, times = n_levels),
    TEXT_LABEL = unlist(levels_map, use.names = FALSE),
    INTEGER_CODE = sequence(n_levels)
  )

  # 3. Fast mapping and mutation -----------------------------------------------
  encoded_cols <- vector("list", length(target_names))
  for (i in seq_along(target_names)) {
    col_name <- target_names[[i]]
    col_data <- df[[col_name]]
    target_levs <- levels_map[[i]]

    # If factor, match by string conversion to avoid index mismatch
    raw_vals <- if (is.factor(col_data)) as.character(col_data) else col_data

    # Match integer indices, then coerce to character for DBI/ROracle safety
    encoded_cols[[i]] <- as.character(collapse::fmatch(raw_vals, target_levs))
  }

  encoded <- df
  collapse::get_vars(encoded, target_names) <- encoded_cols

  list(
    encoded = encoded,
    codebook = codebook
  )
}


# decode from dst metadata or from internal codebook -----------------------------------------------------

#' Decode Encoded Integer Columns Using DSTmetadata or Packaged Codebook
#'
#' @description
#' Decodes integer-encoded categorical columns into factors with descriptive text
#' labels. If \pkg{DSTmetadata} is installed and available, dynamic codelists are
#' fetched via [DSTmetadata::get_code_list()]. When \pkg{DSTmetadata} is missing
#' or inaccessible (e.g., in an offline environment or on air-gapped production
#' nodes), the function falls back to the package's internal `codebook` data object.
#'
#' @param df A data frame or tibble containing integer-encoded columns.
#' @param named_vec_variables_ids An optional named character vector where names
#'   match column names in `df`, and values represent metadata codelist IDs.
#'   Required when retrieving metadata via \pkg{DSTmetadata}; optional if falling
#'   back entirely on internal `codebook` definitions.
#' @param fallback_codebook A data frame with columns `VARIABLE`, `TEXT_LABEL`,
#'   and `INTEGER_CODE`. Defaults to the package's internal dataset `codebook`.
#'
#' @return An object of the same class as `df` with target integer columns
#'   transformed into labelled factors.
#'
#' @family encoding workflows
#' @export
#'
#' @importFrom purrr map set_names
#' @importFrom dplyr mutate across all_of cur_column filter arrange
#'
#' @examples
#' # Minimal demo dataset with integer-encoded causes
#' lifelines_sample <- tibble::tibble(
#'   PERSON_ID = 101:103,
#'   EVENT_START_CAUSE = c(1L, 2L, 7L),
#'   EVENT_FINAL_CAUSE = c(5L, 1L, 2L)
#' )
#'
#' # Fallback decoding via internal codebook
#' decode_int_to_chars(lifelines_sample)
decode_int_to_chars <- function(
  df,
  named_vec_variables_ids = NULL,
  fallback_codebook = codebook
) {
  # 1. Primary Strategy: DSTmetadata API ---------------------------------------
  has_dst <- requireNamespace("DSTmetadata", quietly = TRUE)

  if (has_dst && !is.null(named_vec_variables_ids)) {
    codelists <- named_vec_variables_ids |>
      purrr::map(~ DSTmetadata::get_code_list(.x, update = 3)) |>
      purrr::set_names(names(named_vec_variables_ids))

    vars_present <- intersect(names(codelists), names(df))

    if (length(vars_present) > 0) {
      decoded_df <- df |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(vars_present),
            ~ {
              col_meta <- codelists[[dplyr::cur_column()]]
              factor(
                .x,
                levels = col_meta$code,
                labels = col_meta$title
              )
            }
          )
        )
      return(decoded_df)
    }
  }

  # 2. Fallback Strategy: Internal Package Codebook ----------------------------
  if (missing(fallback_codebook) || is.null(fallback_codebook)) {
    # Ensure lazy-loaded or exported package data is accessible
    data("codebook", package = utils::packageName(), envir = environment())
    fallback_codebook <- get("codebook", envir = environment())
  }

  # Match variables present in both the target df and fallback codebook
  avail_vars <- unique(fallback_codebook$VARIABLE)
  target_vars <- if (!is.null(named_vec_variables_ids)) {
    intersect(names(named_vec_variables_ids), names(df))
  } else {
    intersect(avail_vars, names(df))
  }

  vars_to_decode <- intersect(target_vars, avail_vars)

  if (length(vars_to_decode) == 0) {
    return(df)
  }

  # Build pre-indexed lookup lists to bypass row-filtering inside mutate()
  lookup_map <- vars_to_decode |>
    purrr::map(
      ~ {
        sub_cb <- fallback_codebook |>
          dplyr::filter(.data$VARIABLE == .x) |>
          dplyr::arrange(.data$INTEGER_CODE)

        list(
          levels = as.character(sub_cb$INTEGER_CODE),
          labels = as.character(sub_cb$TEXT_LABEL)
        )
      }
    ) |>
    purrr::set_names(vars_to_decode)

  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(vars_to_decode),
        ~ {
          meta <- lookup_map[[dplyr::cur_column()]]
          factor(
            as.character(.x),
            levels = meta$levels,
            labels = meta$labels
          )
        }
      )
    )
}
