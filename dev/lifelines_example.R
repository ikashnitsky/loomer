#' Ficticious lifelines data
#'
#' A dataset containing 100 fake lifelines, Each observation is a timespan someone was living in Denmark. It is shown how they entered the population and how they exited it.
#'
#' @format A tibble with 100 rows and 12 coloumns
#' \describe{
#'   \item{PERSON_ID}{An integer indicating a person}
#'   \item{BIRTHDAY}{Date of birth}
#'   \item{EVENT_START_DATE}{Date of the starting event}
#'   \item{EVENT_CAUSE_START}{The starting event}
#'   \item{QUALITY_INFORMATION_START}{Quality of the start information}
#'   \item{KILDE_START}{Source for the starting information}
#'   \item{EVENT_FINAL_DATE}{Date of the final event}
#'   \item{EVENT_CAUSE_FINAL}{The final event}
#'   \item{QUALITY_INFORMATION_FINAL}{Quality of the final information}
#'   \item{KILDE_FINAL}{Source for the final information}
#'   \item{NUMBER_EVENT_PERSON}{Number of lines for the person_id}
#'   \item{VERSION}{Date for the last update of the data}
#'   }
#'
"lifelines_example"
