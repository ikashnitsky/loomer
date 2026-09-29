# construct cohort -------------------------------------------------------------

#' Construct Cohort from Register Lifelines
#'
#' @description
#' Filters and slices longitudinal individual register lifelines into a study
#' cohort based on demographic age intervals, calendar study windows, and
#' continuous pre- and post-entry exposure criteria.
#'
#' @param lifelines_dataset A data frame, tibble, or a character string pointing
#'   to a valid Parquet file on disk to be read with \pkg{nanoparquet}. Must contain
#'   at least the following 6 columns: `PERSON_ID`, `BIRTHDAY`, `EVENT_START_DATE`,
#'   `EVENT_START_CAUSE`, `EVENT_FINAL_DATE`, and `EVENT_FINAL_CAUSE`.
#' @param age A numeric vector of length 2 defining the age interval
#'   `c(min_age, max_age)`. Defaults to `c(-1000, 1000)` (unbounded).
#' @param date A vector of class `Date` of length 2 defining the calendar study
#'   window `c(start_date, end_date)`.
#' @param open Logical. If `TRUE` (the default), individuals can enter the study
#'   at any point between `date[1]` and `date[2]`. If `FALSE`, requires presence
#'   at baseline on `date[1]`.
#' @param post_entry_y Numeric scalar. Minimum years of continuous observation
#'   required after study entrance. Defaults to `0`.
#' @param pre_entry_y Numeric scalar. Minimum years in Denmark prior to study
#'   entrance. Defaults to `0`.
#' @param multi_lines Logical. If `TRUE`, allows re-entry into the cohort across
#'   distinct lifeline segments. Defaults to `FALSE` (keeps earliest entry per person).
#' @param all Logical reserved flag for legacy compatibility. Defaults to `FALSE`.
#' @param plot Logical. If `TRUE`, plots a Lexis segment diagram for a sample
#'   of up to 1,000 individuals using \pkg{ggplot2}. Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE` (the default), prints audit logs and parameters.
#'
#' @return A `data.frame` containing the derived cohort lifelines along with
#'   calculated timing markers (`STUDY_ENTRANCE`, `STUDY_EXIT`, `PRE_ENTRY_Y`,
#'   `POST_ENTRY_Y`, and `N_SEGMENTS`).
#'
#' @family cohort workflows
#' @export
#'
#' @importFrom collapse fmutate fselect fsubset roworder funique fnobs
#' @importFrom kit iif
#' @importFrom stringfish convert_to_sf
#' @importFrom nanoparquet read_parquet
#' @importFrom ggplot2 ggplot geom_segment aes ylab xlab theme_light
construct_cohort <- function(
  lifelines_dataset,
  age = NULL,
  date = NULL,
  open = TRUE,
  post_entry_y = 0,
  pre_entry_y = 0,
  multi_lines = FALSE,
  all = FALSE,
  plot = FALSE,
  verbose = TRUE
) {
  # 1. Input Checks ------------------------------------------------------------
  if (verbose) {
    cat("Performing input checks\n")
  }

  if (missing(lifelines_dataset) || is.null(lifelines_dataset)) {
    stop(
      "Argument 'lifelines_dataset' must be provided as a data frame or Parquet file path.",
      call. = FALSE
    )
  }

  if (!is.null(age)) {
    if (!is.numeric(age) || length(age) != 2L) {
      stop(
        "Argument 'age' must be a numeric vector of length 2.",
        call. = FALSE
      )
    }
  } else {
    age <- c(-1000, 1000)
  }

  if (!is.null(date)) {
    if (!is.logical(open) || length(open) != 1L) {
      stop("Argument 'open' must be a single logical value.", call. = FALSE)
    }
    if (!inherits(date, "Date") || length(date) != 2L) {
      stop(
        "Argument 'date' must be a vector of class Date and length 2: c(as.Date('YYYY-MM-DD'), as.Date('YYYY-MM-DD')).",
        call. = FALSE
      )
    }
    if (date[1] > date[2]) {
      stop(
        "The beginning of study (date[1]) must be on or before the end of study (date[2]).",
        call. = FALSE
      )
    }
    study_start_int <- as.integer(unclass(date[1]))
    study_end_int <- as.integer(unclass(date[2]))
  } else {
    study_start_int <- -2147483647L # Equivalent to -Inf for integer Julian dates
    study_end_int <- 2147483647L # Equivalent to Inf
  }

  if (!is.numeric(post_entry_y) || length(post_entry_y) != 1L) {
    stop(
      "Argument 'post_entry_y' must be a single numeric value.",
      call. = FALSE
    )
  }
  if (!is.numeric(pre_entry_y) || length(pre_entry_y) != 1L) {
    stop(
      "Argument 'pre_entry_y' must be a single numeric value.",
      call. = FALSE
    )
  }

  # 2. Ingest Lifelines Dataset ------------------------------------------------
  req_cols <- c(
    "PERSON_ID",
    "BIRTHDAY",
    "EVENT_START_DATE",
    "EVENT_START_CAUSE",
    "EVENT_FINAL_DATE",
    "EVENT_FINAL_CAUSE"
  )

  if (is.character(lifelines_dataset) && length(lifelines_dataset) == 1L) {
    if (!file.exists(lifelines_dataset)) {
      stop(
        sprintf("Parquet file does not exist at path: %s", lifelines_dataset),
        call. = FALSE
      )
    }
    if (verbose) {
      cat(sprintf("Reading Parquet dataset from: %s\n", lifelines_dataset))
    }
    # Read via nanoparquet; col_select performs column pruning at parquet layer
    ll <- nanoparquet::read_parquet(lifelines_dataset)
  } else if (is.data.frame(lifelines_dataset)) {
    ll <- lifelines_dataset
  } else {
    stop(
      "Argument 'lifelines_dataset' must be either a data.frame or a file path string.",
      call. = FALSE
    )
  }

  # Normalize column names to upper case
  names(ll) <- toupper(names(ll))

  missing_cols <- setdiff(req_cols, names(ll))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf(
        "The following required columns are missing from 'lifelines_dataset': %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Coerce to plain data.frame to eliminate overhead in collapse internals
  ll <- as.data.frame(ll)

  # Calculate N_SEGMENTS if absent using collapse::fnobs grouped by PERSON_ID
  if (!"N_SEGMENTS" %in% names(ll)) {
    if (verbose) {
      cat("Calculating missing 'N_SEGMENTS' per PERSON_ID\n")
    }
    ll$N_SEGMENTS <- collapse::fnobs(ll$PERSON_ID, g = ll$PERSON_ID)
  }

  # Ensure Date storage
  ll$BIRTHDAY <- as.Date(ll$BIRTHDAY)
  ll$EVENT_START_DATE <- as.Date(ll$EVENT_START_DATE)
  ll$EVENT_FINAL_DATE <- as.Date(ll$EVENT_FINAL_DATE)

  # ALTREP-wrap character columns via stringfish for C++ std::string backing
  ll$PERSON_ID <- stringfish::convert_to_sf(as.character(ll$PERSON_ID))
  ll$EVENT_START_CAUSE <- stringfish::convert_to_sf(as.character(
    ll$EVENT_START_CAUSE
  ))
  ll$EVENT_FINAL_CAUSE <- stringfish::convert_to_sf(as.character(
    ll$EVENT_FINAL_CAUSE
  ))

  # 3. Scalar Day Conversions --------------------------------------------------
  pre_entry_days <- as.integer(round(pre_entry_y * 365.2425))
  post_entry_days <- as.integer(round(post_entry_y * 365.2425))
  age_lo_days <- as.integer(round(age[1] * 365.2425))
  age_hi_days <- as.integer(round((age[2] + 1L) * 365.2425)) - 1L

  # 4. Vectorized Date Arithmetic (collapse) -----------------------------------
  ll <- collapse::fmutate(
    ll,
    `_bday_int` = unclass(BIRTHDAY),
    `_esd_int` = unclass(EVENT_START_DATE),
    `_efd_int` = unclass(EVENT_FINAL_DATE),

    START_DATE_PRE_ENTRY = structure(
      `_esd_int` + pre_entry_days,
      class = "Date"
    ),
    START_DATE_AGE = structure(`_bday_int` + age_lo_days, class = "Date"),
    FINAL_DATE_AGE = structure(`_bday_int` + age_hi_days, class = "Date"),
    START_DATE_STUDY = structure(study_start_int, class = "Date"),
    FINAL_DATE_STUDY = structure(study_end_int, class = "Date"),

    STUDY_ENTRANCE = structure(
      pmax(
        `_esd_int` + pre_entry_days,
        `_bday_int` + age_lo_days,
        study_start_int
      ),
      class = "Date"
    ),
    STUDY_EXIT = structure(
      pmin(`_efd_int`, `_bday_int` + age_hi_days, study_end_int),
      class = "Date"
    )
  )

  ll <- collapse::fselect(ll, -`_bday_int`, -`_esd_int`, -`_efd_int`)

  # Filter post-entry risk exposure threshold
  ll <- collapse::fsubset(
    ll,
    unclass(STUDY_EXIT) >= unclass(STUDY_ENTRANCE) + post_entry_days
  )

  # Enforce closed cohort constraint (entrance must be on or before baseline start)
  if (!open && !is.null(date)) {
    ll <- collapse::fsubset(
      ll,
      unclass(STUDY_ENTRANCE) <= study_start_int
    )
  }

  # Fast C radix sort
  ll <- collapse::roworder(ll, PERSON_ID, EVENT_START_DATE)

  # De-duplicate to first valid entrance per person unless multi_lines is requested
  if (!multi_lines) {
    ll <- collapse::funique(ll, cols = "PERSON_ID")
  }

  # 5. Derived Event Flags -----------------------------------------------------
  ll$EVENT_START <- kit::iif(
    ll$STUDY_ENTRANCE == ll$EVENT_START_DATE,
    ll$EVENT_START_CAUSE,
    "Lifelines"
  )
  ll$EVENT_END <- kit::iif(
    ll$STUDY_EXIT == ll$EVENT_FINAL_DATE,
    ll$EVENT_FINAL_CAUSE,
    "Lifelines"
  )

  ll$POST_ENTRY_Y <- as.numeric(ll$STUDY_EXIT - ll$STUDY_ENTRANCE) / 365.2425
  ll$PRE_ENTRY_Y <- as.numeric(ll$STUDY_ENTRANCE - ll$EVENT_START_DATE) /
    365.2425

  # 6. Audit & Diagnostics -----------------------------------------------------
  if (verbose) {
    print_age <- sprintf(
      "With an age between %s and %s years when considered in the study\n",
      age[1],
      age[2]
    )
    print_date <- if (!is.null(date)) {
      if (open) {
        sprintf(
          "Who was alive in Denmark at some point between %s and %s\n",
          date[1],
          date[2]
        )
      } else {
        sprintf("Who was in DK on %s\n", date[1])
      }
    } else {
      sprintf(
        "Who was in Denmark between %s and %s\n",
        min(ll$EVENT_START_DATE),
        max(ll$EVENT_FINAL_DATE)
      )
    }
    print_risk <- sprintf(
      "Who has been in the study continuously for %s years or more\n",
      post_entry_y
    )
    print_pre_entry <- sprintf(
      "Who was in Denmark for at least %s years before entering the study\n",
      pre_entry_y
    )
    print_multi <- if (!multi_lines) {
      "Re-entry into study is not allowed\n"
    } else {
      "Re-entry into study allowed\n"
    }

    cat("###########################################\n")
    cat("Creating a cohort of people in Denmark\n")
    cat(print_age, print_date, print_risk, print_pre_entry, print_multi)
    cat("###########################################\n")
  }

  # 7. Diagnostic Plotting -----------------------------------------------------
  if (plot) {
    if (verbose) {
      cat("Plotting cohort\n")
    }
    n_rows <- nrow(ll)
    if (n_rows > 1000L) {
      warning(
        "Cohort too big to plot. A random sample of 1,000 lifelines is plotted.",
        call. = FALSE
      )
      plot_df <- ll[sample.int(n_rows, 1000L), ]
      n_rows <- 1000L
    } else {
      plot_df <- ll
    }

    p <- ggplot2::ggplot(plot_df) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = .data$STUDY_ENTRANCE,
          xend = .data$STUDY_EXIT,
          y = as.numeric(.data$STUDY_ENTRANCE - .data$BIRTHDAY) / 365.2425,
          yend = as.numeric(.data$STUDY_EXIT - .data$BIRTHDAY) / 365.2425
        ),
        linewidth = 1,
        col = "lightblue",
        alpha = 1 - (0.8 * n_rows / 1000)
      ) +
      ggplot2::ylab("Age (years)") +
      ggplot2::xlab("Date") +
      ggplot2::theme_light()

    print(p)
  }

  if (verbose) {
    cat("DONE\n")
  }

  ll
}
