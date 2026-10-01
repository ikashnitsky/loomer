# 1. to_yyyymmdd_iso_txt() tests ------------------------------------------------
#
# to_yyyymmdd_iso_txt() encodes date columns for safe transmission/storage: it
# finds every Date and POSIXct column in the frame and rewrites it as a plain
# character string in "YYYY-MM-DD" form. Non-date columns are left untouched.

test_that("to_yyyymmdd_iso_txt converts Date and POSIXct to YYYY-MM-DD character", {
  # Build a frame with one Date column, one POSIXct column, and two
  # non-date columns (an integer id and a character label) to confirm
  # that only the date-like columns are reformatted.
  input_df <- tibble::tibble(
    id = 1:2,
    event_date = as.Date(c("2026-01-15", "2026-07-23")),
    event_time = as.POSIXct(
      c("2026-01-15 10:30:00", "2026-07-23 23:59:59"),
      tz = "UTC"
    ),
    label = c("A", "B")
  )

  res <- to_yyyymmdd_iso_txt(input_df)

  # Both date columns become character...
  expect_type(res$event_date, "character")
  expect_type(res$event_time, "character")
  # ...formatted as YYYY-MM-DD. Note the POSIXct keeps only the date part,
  # so both times collapse to the same calendar date string.
  expect_equal(res$event_date, c("2026-01-15", "2026-07-23"))
  expect_equal(res$event_time, c("2026-01-15", "2026-07-23"))
  # Non-date columns pass through completely unchanged.
  expect_equal(res$label, c("A", "B"))
  expect_equal(res$id, 1:2)
})

test_that("to_yyyymmdd_iso_txt preserves NA values across Date and POSIXct", {
  # NAs must survive the conversion as NA_character_, not become the literal
  # string "NA" or a formatted empty value.
  input_df <- tibble::tibble(
    na_date = as.Date(c("2026-01-01", NA)),
    na_time = as.POSIXct(c(NA, "2026-05-10 12:00:00"), tz = "UTC")
  )

  res <- to_yyyymmdd_iso_txt(input_df)

  expect_equal(res$na_date, c("2026-01-01", NA_character_))
  expect_equal(res$na_time, c(NA_character_, "2026-05-10"))
})

test_that("to_yyyymmdd_iso_txt passes through data unchanged if no dates exist", {
  # When there are no Date/POSIXct columns, the function should be a no-op
  # and return an identical frame. The character "2026-01-01" is a string,
  # not a Date, so it must NOT be touched (encoding only acts on typed dates).
  input_df <- tibble::tibble(
    num = c(1L, 2L),
    txt = c("2026-01-01", "unrelated")
  )

  res <- to_yyyymmdd_iso_txt(input_df)

  expect_equal(res, input_df)
})


# 2. from_yyyymmdd_iso_txt() tests ----------------------------------------------
#
# from_yyyymmdd_iso_txt() is the inverse of the encoder: it turns character
# columns back into Date vectors. By default it auto-detects target columns
# using a simple heuristic — the column's FIRST element is a non-NA string of
# exactly 10 chars (i.e. "YYYY-MM-DD"). You can also pass explicit column
# names via date_cols to override that heuristic.

test_that("from_yyyymmdd_iso_txt correctly detects and converts 10-char ISO strings", {
  # Two columns of exact "YYYY-MM-DD" strings (should convert), one short
  # string column and one datetime-length string column (both should be
  # ignored, because their first elements are not exactly 10 chars).
  input_df <- tibble::tibble(
    id = 1:2,
    iso_date = c("2026-03-01", "2026-11-20"),
    iso_date2 = c("1999-12-31", "2000-01-01"),
    short_str = c("ABC", "DEF"),
    long_str = c("2026-03-01 12:00", "2026-11-20 00:00")
  )

  res <- from_yyyymmdd_iso_txt(input_df)

  # Auto-detected columns are now Date-typed and hold the right values.
  expect_s3_class(res$iso_date, "Date")
  expect_s3_class(res$iso_date2, "Date")
  expect_equal(res$iso_date, as.Date(c("2026-03-01", "2026-11-20")))
  expect_equal(res$iso_date2, as.Date(c("1999-12-31", "2000-01-01")))

  # Non-target columns (different char lengths on row 1) must remain untouched
  expect_type(res$short_str, "character")
  expect_type(res$long_str, "character")
})

test_that("from_yyyymmdd_iso_txt respects explicit date_cols argument", {
  # Both columns look like ISO dates, so auto-detection would convert both.
  # Passing date_cols should override that and convert only the named column.
  input_df <- tibble::tibble(
    date_a = c("2026-01-01", "2026-01-02"),
    date_b = c("2026-02-01", "2026-02-02")
  )

  # Only convert date_a, leave date_b as character
  res <- from_yyyymmdd_iso_txt(input_df, date_cols = "date_a")

  expect_s3_class(res$date_a, "Date")
  expect_type(res$date_b, "character")
  expect_equal(res$date_a, as.Date(c("2026-01-01", "2026-01-02")))
})

test_that("from_yyyymmdd_iso_txt handles leading NA without crashing column detection", {
  # The auto-detection heuristic reads x[1]. If the first value is NA it
  # cannot infer length, so the column should be safely skipped (no crash,
  # no false positive). Pure-NA and unrelated columns must also be skipped.
  input_df <- tibble::tibble(
    date_with_na = c(NA_character_, "2026-08-15"),
    pure_na = c(NA_character_, NA_character_),
    other = c("abc", "def")
  )

  # Column detection safely ignores vectors starting with NA
  res <- from_yyyymmdd_iso_txt(input_df)

  expect_type(res$date_with_na, "character")
  expect_type(res$pure_na, "character")

  # Manual override still works when NAs are present: with an explicit
  # date_cols we bypass the heuristic and parse the column, preserving NA.
  res_forced <- from_yyyymmdd_iso_txt(input_df, date_cols = "date_with_na")
  expect_s3_class(res_forced$date_with_na, "Date")
  expect_equal(res_forced$date_with_na, as.Date(c(NA, "2026-08-15")))
})


# 3. Round-trip integrity ------------------------------------------------------
#
# Encoding and decoding should be inverse operations: a frame of Dates goes
# out as YYYY-MM-DD strings and back to the exact same Dates, leaving other
# columns (e.g. character ids) untouched. This is the key correctness
# guarantee for the encode/decode pair.

test_that("to_yyyymmdd_iso_txt and from_yyyymmdd_iso_txt round-trip preserves Date values", {
  # Mixed frame: one character id column + two Date columns, mirroring a
  # realistic OMOP-like wide table.
  original_df <- tibble::tibble(
    PERSON_ID = c("P01", "P02", "P03"),
    BIRTHDAY = as.Date(c("1985-04-12", "1992-09-30", "2001-01-05")),
    EVENT_DATE = as.Date(c("2026-01-01", "2026-06-15", "2026-12-31"))
  )

  # Date -> Character ISO -> Date
  iso_encoded <- to_yyyymmdd_iso_txt(original_df)
  roundtripped <- from_yyyymmdd_iso_txt(iso_encoded)

  # The final frame is identical to the original, Date columns included.
  expect_equal(roundtripped, original_df)
})
