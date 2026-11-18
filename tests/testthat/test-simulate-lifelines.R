test_that("output follows the LIFELINES schema", {
  ll <- simulate_lifelines(1000, seed = 1)

  expect_s3_class(ll, "tbl_df")
  expect_named(
    ll,
    c("PERSON_ID", "BIRTHDAY", "EVENT_START_DATE", "EVENT_START_CAUSE",
      "EVENT_FINAL_DATE", "EVENT_FINAL_CAUSE", "N_SEGMENTS")
  )
  expect_type(ll$BIRTHDAY, "character")
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", ll$EVENT_FINAL_DATE)))
  expect_identical(
    levels(ll$EVENT_START_CAUSE),
    as.character(codebook$TEXT_LABEL[codebook$VARIABLE == "EVENT_START_CAUSE"])
  )
  expect_identical(
    levels(ll$EVENT_FINAL_CAUSE),
    as.character(codebook$TEXT_LABEL[codebook$VARIABLE == "EVENT_FINAL_CAUSE"])
  )
  expect_identical(
    unname(ll$N_SEGMENTS),
    as.integer(ave(seq_along(ll$PERSON_ID), ll$PERSON_ID, FUN = length))
  )
  base <- ll$EVENT_START_CAUSE == "Bopael foer 1968"
  expect_equal(sum(base), 1000L)
  expect_true(all(ll$EVENT_START_DATE[base] == "1968-04-02"))
})

test_that("lifelines are chronologically coherent", {
  ll <- simulate_lifelines(2000, date_format = "Date", seed = 2)
  b <- as.integer(ll$BIRTHDAY)
  s <- as.integer(ll$EVENT_START_DATE)
  e <- as.integer(ll$EVENT_FINAL_DATE)

  expect_true(all(s >= b))
  expect_true(all(e >= s))
  expect_true(all(e <= as.integer(as.Date("2025-12-31"))))

  same <- ll$PERSON_ID[-1L] == ll$PERSON_ID[-nrow(ll)]
  expect_true(all(s[-1L][same] > e[-nrow(ll)][same]))
  # only the last segment of a person may be open / a death
  expect_false(any(ll$EVENT_FINAL_CAUSE[-nrow(ll)][same] %in%
                     c("Doed", "I befolkningen")))
  expect_true(all(
    (ll$EVENT_START_CAUSE == "Foedt") <= (s == b)
  ))
})

test_that("seed gives reproducible results and restores the RNG", {
  expect_identical(
    simulate_lifelines(500, seed = 9),
    simulate_lifelines(500, seed = 9)
  )
  set.seed(1)
  r1 <- runif(1)
  set.seed(1)
  invisible(simulate_lifelines(100, seed = 5))
  expect_identical(runif(1), r1)
})

test_that("demographic targets are recovered", {
  skip_on_cran()
  ll <- simulate_lifelines(
    30000, start_date = "1990-01-01", end_date = "2019-12-31",
    e0 = 80, tfr = 2, net_migration = 4, emigration = 6,
    date_format = "Date", include_sex = TRUE, seed = 11
  )
  b <- as.integer(ll$BIRTHDAY)
  s <- as.integer(ll$EVENT_START_DATE)
  e <- as.integer(ll$EVENT_FINAL_DATE)
  py <- sum(e - s + 1) / 365.25

  # crude rates
  imm <- sum(ll$EVENT_START_CAUSE == "Indvandret")
  emi <- sum(ll$EVENT_FINAL_CAUSE == "Udvandret")
  expect_equal((imm - emi) / py * 1000, 4, tolerance = 0.1)
  expect_equal(emi / py * 1000, 6, tolerance = 0.1)

  # TFR via births over female exposure weighted by the fertility schedule
  w <- .asfr_shape(0:110, 30)
  expo_f <- vapply(0:49, function(x) {
    lo <- pmax(s, b + round(x * 365.25))
    hi <- pmin(e + 1, b + round((x + 1) * 365.25))
    sum(pmax(0, hi - lo)[ll$SEX == 2L]) / 365.25
  }, numeric(1L))
  tfr_hat <- sum(ll$EVENT_START_CAUSE == "Foedt") / sum(w[1:50] * expo_f)
  expect_equal(tfr_hat, 2, tolerance = 0.05)

  # parameters attribute
  p <- attr(ll, "parameters")
  expect_equal(nrow(p), 30L)
  expect_equal(p$e0_female - p$e0_male, rep(4, 30))
})

test_that("time-varying parameters are expanded and interpolated", {
  ll <- simulate_lifelines(
    300, start_date = "2000-01-01", end_date = "2009-12-31",
    e0 = c(70, 79), tfr = seq(1.2, 2.1, by = 0.1), net_migration = -20,
    emigration = 5, seed = 3
  )
  p <- attr(ll, "parameters")
  expect_equal(p$e0, seq(70, 79, length.out = 10))
  expect_equal(p$tfr, seq(1.2, 2.1, by = 0.1))
  expect_equal(p$emigration, rep(20, 10))
  expect_equal(p$immigration, rep(0, 10))
  expect_false(any(ll$EVENT_START_CAUSE == "Indvandret"))
})

test_that("mortality level solver hits target e0", {
  mu <- .siler_hazard(0:110 + 0.5)
  for (target in c(40, 65, 80, 95)) {
    th <- .solve_mortality_level(target, mu)
    expect_equal(sum(.Lx(th * mu)), target, tolerance = 1e-8)
  }
})

test_that("invalid inputs are rejected", {
  expect_error(simulate_lifelines(0), "'n'")
  expect_error(simulate_lifelines(10, e0 = 200), "'e0'")
  expect_error(simulate_lifelines(10, tfr = NA), "'tfr'")
  expect_error(
    simulate_lifelines(10, start_date = "2020-01-01", end_date = "2019-01-01"),
    "after"
  )
  expect_error(simulate_lifelines(10, baseline_cause = "foo"), "baseline")
  expect_error(simulate_lifelines(10, admin_rates = c(bad = 1)), "admin")
})
