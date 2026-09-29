#' Codebook for Life Event Cause Codes
#'
#' A reference mapping table documenting integer encodings and factor labels for
#' event start and final termination causes used in longitudinal register data.
#'
#' @format ## `codebook`
#' A data frame (tibble) with 12 rows and 3 columns:
#' \describe{
#'   \item{VARIABLE}{Target variable name (`character`), e.g., `"EVENT_START_CAUSE"` or `"EVENT_FINAL_CAUSE"`.}
#'   \item{TEXT_LABEL}{Human-readable descriptive label for the event cause (`factor`), formatted in Danish.}
#'   \item{INTEGER_CODE}{Assigned integer identifier representing the specific event cause (`integer`).}
#' }
#'
#' @source Register metadata documentation and administrative coding conventions (Statistics Denmark / DST).
#'
#' @examples
#' data(codebook)
#' head(codebook)
#'
#' # Inspect available categories for event start causes
#' subset(codebook, VARIABLE == "EVENT_START_CAUSE")
"codebook"
