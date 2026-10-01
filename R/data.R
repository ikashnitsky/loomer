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


#' Synthetic Register Lifelines Dataset
#'
#' @description
#' A synthetic benchmark dataset of approximately 10,000 observation segments
#' modeling individual-level Danish demographic register histories (`LIFELINES`)
#' from 1986 through 2025. Generated via conditional CART synthesis
#' (\pkg{synthpop}) to preserve multivariate distributions, right-censoring
#' rules, and multi-segment immigration dynamics without disclosing real-world
#' personal data.
#'
#' All date attributes are formatted as ISO 8601 character strings
#' (`"YYYY-MM-DD"`), matching the database schema of `STATPROD`.
#'
#' @format ## `synthetic_lifelines`
#' A tibble with 10,000 rows and 7 columns:
#' \describe{
#'   \item{PERSON_ID}{Synthetic individual identifier (`character`), formatted
#'     as `"SYN_XXXXXXX"`.}
#'   \item{BIRTHDAY}{Date of birth (`character`, ISO 8601 `"YYYY-MM-DD"`).}
#'   \item{EVENT_START_DATE}{Start date of the observation spell (`character`,
#'     ISO 8601 `"YYYY-MM-DD"`).}
#'   \item{EVENT_START_CAUSE}{Administrative entry reason (`factor`) with
#'     levels: `"Foedt foer 1968"`, `"Indvandret"`, `"Bopael"`,
#'     `"Bopael foer 1968"`, `"Indvandret foer 1968"`, `"Fundet"`, `"Foedt"`.}
#'   \item{EVENT_FINAL_DATE}{Termination date of the observation spell
#'     (`character`, ISO 8601 `"YYYY-MM-DD"`).}
#'   \item{EVENT_FINAL_CAUSE}{Administrative exit or censoring reason (`factor`)
#'     with levels: `"Doed"`, `"Udvandret"`, `"Korrektioner"`, `"Forsvundet"`,
#'     `"I befolkningen"`.}
#'   \item{N_SEGMENTS}{Total number of lifetime segments associated with the
#'     given `PERSON_ID` (`integer`). Values greater than `1` represent
#'     individuals with interrupted residency spells (e.g., temporary emigration
#'     and re-immigration).}
#' }
#'
#' @details
#' This dataset complies with the storage specifications of the 2025 `LIFELINES`
#' release in Oracle `STATPROD`. To convert date strings to native R `Date`
#' objects and map categorical variables to target factor schemas, run
#' [from_yyyymmdd_iso_txt()] and [decode_int_to_chars()].
#'
#' @source Synthesized from administrative lifelines patterns using \pkg{synthpop}
#'   and constrained by Danish register demographic boundary rules.
#'
#' @family datasets
#'
#' @examples
#' data(synthetic_lifelines)
#' head(synthetic_lifelines)
#'
#' # Check distribution of re-entries (multiple segments per individual)
#' table(synthetic_lifelines$N_SEGMENTS)
"synthetic_lifelines"
