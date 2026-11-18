toy_ll <- tibble::tibble(
  PERSON_ID = c("A", "B", "C", "D"),
  EVENT_START_DATE = c("2010-05-01", "2015-01-01", "2020-01-02", "1999-12-31"),
  EVENT_FINAL_DATE = c("2020-01-01", "2019-12-31", "2025-12-31", "2025-12-31")
)
toy_ll_date <- from_yyyymmdd_iso_txt(toy_ll)

test_that("population queries give identical results for ISO text and Date", {
  # Regression: character dates were compared lexicographically against the
  # integer day number of the query date, silently returning 0 rows.
  expect_identical(
    in_population_on_date(toy_ll, "2020-01-01")$PERSON_ID,
    c("A", "D")
  )
  expect_identical(
    in_population_on_date(toy_ll_date, "2020-01-01")$PERSON_ID,
    c("A", "D")
  )

  dates <- as.Date(c("2000-01-01", "2016-06-01", "2020-01-01", "2024-01-01"))
  expected <- c(1L, 3L, 2L, 2L)
  expect_identical(total_pop_on_date(toy_ll, dates)$pop, expected)
  expect_identical(total_pop_on_date(toy_ll_date, dates)$pop, expected)
})

test_that("population queries work on the packaged synthetic data", {
  ll_date <- from_yyyymmdd_iso_txt(synthetic_lifelines)
  dates <- as.Date(c("2000-01-01", "2020-01-01"))

  pop_txt <- total_pop_on_date(synthetic_lifelines, dates)$pop
  expect_true(all(pop_txt > 0L))
  expect_identical(pop_txt, total_pop_on_date(ll_date, dates)$pop)
  expect_identical(
    nrow(in_population_on_date(synthetic_lifelines, "2020-01-01")),
    pop_txt[2]
  )
})

test_that("POSIXct date columns are supported and bad types are rejected", {
  ll_posix <- toy_ll_date
  ll_posix$EVENT_START_DATE <- as.POSIXct(ll_posix$EVENT_START_DATE)
  expect_identical(total_pop_on_date(ll_posix, "2020-01-01")$pop, 2L)

  ll_bad <- toy_ll
  ll_bad$EVENT_START_DATE <- 1:4
  expect_error(total_pop_on_date(ll_bad, "2020-01-01"), "EVENT_START_DATE")
})
