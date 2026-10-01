# in_population_on_date ---------------------------------------------------------

#' Filter Lifelines Active on a Specific Date
#'
#' @description
#' Subsets a longitudinal lifelines dataset to individuals present in the
#' population on a given reference date (where `EVENT_START_DATE <= date` and
#' `EVENT_FINAL_DATE >= date`).
#'
#' @param data A data frame or tibble containing register lifelines. Column
#'   names are matched case-insensitively for `EVENT_START_DATE` and
#'   `EVENT_FINAL_DATE`.
#' @param date An object coercible to `Date` (e.g., `"YYYY-MM-DD"` string or `Date`).
#'
#' @return A subset of `data` of the same class containing active records on `date`.
#'
#' @family population queries
#' @export
#'
#' @importFrom collapse fsubset
#' @importFrom fastymd fymd
#'
#' @examples
#' data(synthetic_lifelines, package = "loomer")
#' # Filter active residents on a specific census date
#' active_at_census <- in_population_on_date(synthetic_lifelines, "2020-01-01")
#' nrow(active_at_census)
in_population_on_date <- function(data, date) {
  if (missing(data) || is.null(data)) {
    stop("Argument 'data' must be provided.", call. = FALSE)
  }
  if (missing(date) || length(date) != 1L) {
    stop("Argument 'date' must be a scalar date or string.", call. = FALSE)
  }

  target_date <- if (inherits(date, "Date")) {
    date
  } else {
    fastymd::fymd(as.character(date))
  }
  target_int <- unclass(target_date)

  # Support upper or lower case column names seamlessly
  cnames <- names(data)
  start_var <- cnames[toupper(cnames) == "EVENT_START_DATE"]
  final_var <- cnames[toupper(cnames) == "EVENT_FINAL_DATE"]

  if (length(start_var) != 1L || length(final_var) != 1L) {
    stop(
      "Input 'data' must contain 'EVENT_START_DATE' and 'EVENT_FINAL_DATE' columns.",
      call. = FALSE
    )
  }

  # Fast C-level integer comparison via collapse::fsubset
  collapse::fsubset(
    data,
    unclass(data[[start_var]]) <= target_int &
      unclass(data[[final_var]]) >= target_int
  )
}


# total_pop_on_date ------------------------------------------------------------

#' Calculate Total Population Size on Specific Date(s)
#'
#' @description
#' Calculates the total population count active on one or multiple reference
#' dates. Evaluates numeric integer bounds directly without constructing
#' intermediate filtered subsets.
#'
#' @param data A data frame, tibble, or `data.table` containing register lifelines.
#' @param date A vector of dates (or `"YYYY-MM-DD"` strings). Supports single dates
#'   or multiple dates (e.g., annual census series).
#'
#' @return A [tibble::tibble()] with two columns:
#' \describe{
#'   \item{`date`}{The reference date (`Date`).}
#'   \item{`pop`}{Count of active individuals (`integer`).}
#' }
#'
#' @family population queries
#' @export
#'
#' @importFrom fastymd fymd
#' @importFrom kit count
#' @importFrom tibble tibble
#'
#' @examples
#' data(synthetic_lifelines, package = "loomer")
#' # Query single date population count
#' total_pop_on_date(synthetic_lifelines, "2020-01-01")
#'
#' # Compute annual census counts across a 10-year span
#' census_dates <- as.Date(sprintf("%d-01-01", 2015:2025))
#' pop_series <- total_pop_on_date(synthetic_lifelines, census_dates)
#' print(pop_series)
# Rectified function in R/population.R
total_pop_on_date <- function(data, date) {
  if (missing(data) || is.null(data)) {
    stop("Argument 'data' must be provided.", call. = FALSE)
  }
  if (missing(date) || length(date) == 0L) {
    stop("Argument 'date' cannot be empty.", call. = FALSE)
  }

  target_dates <- if (inherits(date, "Date")) {
    date
  } else {
    fastymd::fymd(as.character(date))
  }
  target_ints <- unclass(target_dates)

  cnames <- names(data)
  start_var <- cnames[toupper(cnames) == "EVENT_START_DATE"]
  final_var <- cnames[toupper(cnames) == "EVENT_FINAL_DATE"]

  if (length(start_var) != 1L || length(final_var) != 1L) {
    stop(
      "Input 'data' must contain 'EVENT_START_DATE' and 'EVENT_FINAL_DATE' columns.",
      call. = FALSE
    )
  }

  start_int <- unclass(data[[start_var]])
  final_int <- unclass(data[[final_var]])

  # kit::count requires both 'x' and 'value' -> count(x, value = TRUE)
  counts <- vapply(
    target_ints,
    function(ti) {
      kit::count(start_int <= ti & final_int >= ti, value = TRUE)
    },
    integer(1L),
    USE.NAMES = FALSE
  )

  tibble::tibble(
    date = target_dates,
    pop = counts
  )
}
