# ..........................................................
# 2026-09-29 -- loomer
# internal data                -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

#  save codelists into the package ---------------------------------------

library(tidyverse)
library(nanoparquet)

codebook <- read_parquet("K:/Lifelines/prq/lifelines_codebook.parquet")

codebook |> usethis::use_data()


# synthetic 10k lifelines ------------------------------------------------

# Load core packages
library(synthpop)
library(tidyverse)
library(clock)
library(collapse)
library(kit)

# 1. Construct representative seed data matching LIFELINES schema --------------
set.seed(911)
n_seed_persons <- 1500L

# Base demographic attributes
seed_persons <- tibble::tibble(
  PERSON_ID = sprintf("ID_%05d", seq_len(n_seed_persons)),
  BIRTHDAY = as.Date("1940-01-01") +
    sample.int(22000L, size = n_seed_persons, replace = TRUE),
  # Higher probability of 1 segment, decreasing for multiple migrations
  n_segments = sample(
    1:3,
    size = n_seed_persons,
    replace = TRUE,
    prob = c(0.85, 0.12, 0.03)
  )
)

# Expand segments per person
seed_segments <- seed_persons |>
  tidyr::uncount(.data$n_segments, .id = "segment_idx")

# Generate chronologically coherent event sequences per segment
seed_lifelines <- seed_segments |>
  dplyr::group_by(.data$PERSON_ID) |>
  dplyr::mutate(
    # Start dates depend on birth and segment index
    gap_days = sample(365:3650, size = dplyr::n(), replace = TRUE),
    duration_days = sample(180:5000, size = dplyr::n(), replace = TRUE),

    EVENT_START_DATE = if_else(
      .data$segment_idx == 1L,
      .data$BIRTHDAY + as.integer(round(sample(0:15000, size = 1L))),
      as.Date("1986-01-01") + cumsum(.data$gap_days)
    ),
    EVENT_FINAL_DATE = pmin(
      .data$EVENT_START_DATE + .data$duration_days,
      as.Date("2025-12-31")
    ),
    EVENT_START_CAUSE = factor(
      if_else(
        .data$segment_idx == 1L & .data$EVENT_START_DATE == .data$BIRTHDAY,
        "Foedt",
        sample(
          c("Bopael", "Indvandret", "Fundet"),
          size = dplyr::n(),
          replace = TRUE
        )
      ),
      levels = c(
        "Foedt foer 1968",
        "Indvandret",
        "Bopael",
        "Bopael foer 1968",
        "Indvandret foer 1968",
        "Fundet",
        "Foedt"
      )
    ),
    EVENT_FINAL_CAUSE = factor(
      if_else(
        .data$EVENT_FINAL_DATE == as.Date("2025-12-31"),
        "I befolkningen",
        sample(
          c("Doed", "Udvandret", "Korrektioner", "Forsvundet"),
          size = dplyr::n(),
          replace = TRUE
        )
      ),
      levels = c(
        "Doed",
        "Udvandret",
        "Korrektioner",
        "Forsvundet",
        "I befolkningen"
      )
    )
  ) |>
  dplyr::ungroup()

# Add integer representations of dates to guide synthpop conditional trees
seed_prep <- seed_lifelines |>
  collapse::fmutate(
    BIRTHDAY_INT = unclass(BIRTHDAY),
    START_INT = unclass(EVENT_START_DATE),
    FINAL_INT = unclass(EVENT_FINAL_DATE)
  ) |>
  collapse::fselect(
    BIRTHDAY_INT,
    START_INT,
    FINAL_INT,
    EVENT_START_CAUSE,
    EVENT_FINAL_CAUSE
  ) |>
  as.data.frame()

# 2. Synthesize ~10k records using Cart-based synthesis -------------------------
# Define visit sequence preserving chronological DAG:
# Birth -> Segment Start -> Segment End -> Entry Cause -> Exit Cause
synth_visit_seq <- c(
  "BIRTHDAY_INT",
  "START_INT",
  "FINAL_INT",
  "EVENT_START_CAUSE",
  "EVENT_FINAL_CAUSE"
)

synth_out <- synthpop::syn(
  data = seed_prep,
  method = "cart",
  visit.sequence = synth_visit_seq,
  k = 10000L,
  seed = 911
)

# 3. Post-process synthetic output back into loomer / register schema: Synthesize, enforce administrative rules, and format lifelines schema --------
synthetic_lifelines <- synth_out$syn |>
  tibble::as_tibble() |>
  # 1. Enforce chronological boundaries and restore Date S3 class
  collapse::fmutate(
    START_INT = pmax(START_INT, BIRTHDAY_INT),
    FINAL_INT = pmin(
      pmax(FINAL_INT, START_INT),
      unclass(as.Date("2025-12-31"))
    ),
    BIRTHDAY = structure(as.integer(BIRTHDAY_INT), class = "Date"),
    EVENT_START_DATE = structure(as.integer(START_INT), class = "Date"),
    EVENT_FINAL_DATE = structure(as.integer(FINAL_INT), class = "Date")
  ) |>
  # 2. Derive synthetic PERSON_ID sequences
  dplyr::mutate(
    person_marker = sample(
      c(0L, 1L),
      size = dplyr::n(),
      replace = TRUE,
      prob = c(0.15, 0.85)
    ),
    PERSON_ID = sprintf("SYN_%07d", cumsum(person_marker))
  ) |>
  collapse::roworder(PERSON_ID, EVENT_START_DATE) |>
  # 3. Synchronize administrative causes, compute segment counts, and ISO format
  collapse::fmutate(
    EVENT_FINAL_CAUSE = factor(
      kit::iif(
        EVENT_FINAL_DATE == as.Date("2025-12-31"),
        "I befolkningen",
        as.character(EVENT_FINAL_CAUSE)
      ),
      levels = levels(EVENT_FINAL_CAUSE)
    ),
    EVENT_START_CAUSE = factor(
      kit::iif(
        EVENT_START_DATE == BIRTHDAY,
        "Foedt",
        as.character(EVENT_START_CAUSE)
      ),
      levels = levels(EVENT_START_CAUSE)
    )
  ) |>
  group_by(PERSON_ID) |>
  dplyr::mutate(
    N_SEGMENTS = n()
  ) |>
  ungroup() |>
  collapse::fselect(
    PERSON_ID,
    BIRTHDAY,
    EVENT_START_DATE,
    EVENT_START_CAUSE,
    EVENT_FINAL_DATE,
    EVENT_FINAL_CAUSE,
    N_SEGMENTS
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::where(\(x) inherits(x, "Date")),
      ~ clock::date_format(.x, format = "%Y-%m-%d")
    )
  )

# save the synthetic dataset
usethis::use_data(synthetic_lifelines, overwrite = TRUE, compress = "xz")
