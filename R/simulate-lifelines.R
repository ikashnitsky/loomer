# simulate_lifelines -----------------------------------------------------------

#' Simulate a Synthetic Lifelines Population
#'
#' @description
#' Generates a synthetic register `LIFELINES` dataset of arbitrary size via a
#' vectorised discrete-time (annual) demographic microsimulation. Births,
#' deaths, emigration, immigration, return migration and administrative
#' events (disappearance, re-finding, corrections) are simulated individually,
#' and every residence spell is written out as one lifeline segment using the
#' same schema and factor levels as [synthetic_lifelines] and [codebook].
#'
#' Demographic intensity is controlled with three classic summary indicators
#' that may be constant or vary over calendar time:
#'
#' * `e0` -- period life expectancy at birth (both sexes);
#' * `tfr` -- period total fertility rate;
#' * `net_migration` -- crude net migration rate per 1,000 population.
#'
#' @details
#' **Model schedules.**
#' * *Mortality*: a Siler (1979) hazard of fixed shape is rescaled
#'   proportionally, separately for each sex and calendar year, so that the
#'   implied life table hits the target life expectancy exactly
#'   (`e0 +/- e0_sex_gap / 2` for females / males).
#' * *Fertility*: a Gaussian-shaped age-specific fertility schedule on ages
#'   15--49 with mean age at childbearing `mac`, scaled to `tfr`. The sex ratio
#'   at birth is 1.05.
#' * *Migration*: a standard Rogers--Castro (1981) model schedule defines both
#'   the relative age profile of emigration risk and the age distribution of
#'   new immigrants. The gross emigration rate is `emigration` per 1,000;
#'   the gross immigration flow is `emigration + net_migration` per 1,000 so
#'   that the net crude rate equals `net_migration`. A share `return_share` of
#'   immigrants is drawn from former emigrants, producing multi-segment
#'   lifelines (`N_SEGMENTS > 1`).
#'
#' **Time-varying parameters.** `e0`, `tfr`, `net_migration`, and `emigration`
#' accept (a) a scalar, (b) a vector with one value per simulated calendar year,
#' or (c) a shorter vector of anchor values that is linearly interpolated over
#' the simulation horizon -- e.g. `e0 = c(73, 82)` gives a linear increase from
#' 73 in the first year to 82 in the last.
#'
#' **Baseline population.** The `n` persons alive on `start_date` are drawn
#' from the stable age structure implied by the first-year parameters and get
#' `EVENT_START_DATE = start_date` and `EVENT_START_CAUSE = baseline_cause`.
#' With the default `start_date` (CPR introduction, 1968-04-02) this mirrors
#' the left-truncated stock of the real register.
#'
#' Entry causes generated: `baseline_cause`, `"Foedt"`, `"Indvandret"`,
#' `"Fundet"`. Exit causes generated: `"Doed"`, `"Udvandret"`,
#' `"Forsvundet"`, `"Korrektioner"`, `"I befolkningen"` (right-censored at
#' `end_date`).
#'
#' The realised annual parameter trajectories are attached to the output as
#' attribute `"parameters"`.
#'
#' @param n Integer. Size of the population alive on `start_date`.
#' @param start_date,end_date Simulation window, `Date` or `"YYYY-MM-DD"`.
#' @param e0 Numeric. Life expectancy at birth, both sexes (years). Scalar,
#'   per-year vector or interpolation anchors (see Details).
#' @param tfr Numeric. Total fertility rate (children per woman).
#' @param net_migration Numeric. Crude net migration rate per 1,000 population
#'   per year. May be negative.
#' @param emigration Numeric. Gross crude emigration rate per 1,000 population
#'   per year. If `net_migration < -emigration` emigration is raised to
#'   `-net_migration` (zero immigration).
#' @param mac Numeric scalar. Mean age at childbearing.
#' @param e0_sex_gap Numeric scalar. Female minus male life expectancy.
#' @param return_share Numeric in `[0, 1]`. Share of immigrants who are
#'   returning former emigrants (as far as the pool of emigrants permits).
#' @param admin_rates Named numeric vector of administrative event
#'   intensities per person-year: `disappear` (exit as `"Forsvundet"`),
#'   `found` (annual hazard that a disappeared person re-enters as
#'   `"Fundet"`), and `correction` (exit as `"Korrektioner"`). Set to `0` to
#'   switch off.
#' @param baseline_cause Character. Entry cause assigned to the baseline stock;
#'   one of the `EVENT_START_CAUSE` levels in [codebook].
#' @param date_format Either `"iso"` (default; `"YYYY-MM-DD"` character, as in
#'   [synthetic_lifelines]) or `"Date"`.
#' @param include_sex Logical. Append a `SEX` column (`1` = male,
#'   `2` = female)? Defaults to `FALSE` to match the `LIFELINES` schema.
#' @param seed Optional integer seed for reproducibility. The global RNG state
#'   is restored on exit.
#'
#' @return A [tibble::tibble()] with columns `PERSON_ID`, `BIRTHDAY`,
#'   `EVENT_START_DATE`, `EVENT_START_CAUSE`, `EVENT_FINAL_DATE`,
#'   `EVENT_FINAL_CAUSE`, `N_SEGMENTS` (and optionally `SEX`), sorted by person
#'   and spell start, with attribute `"parameters"`.
#'
#' @references
#' Siler, W. (1979). A competing-risk model for animal mortality. *Ecology*,
#' 60(4), 750--757.
#'
#' Rogers, A., & Castro, L. J. (1981). *Model migration schedules*. IIASA
#' Research Report RR-81-30.
#'
#' @family datasets
#' @export
#'
#' @importFrom stats runif rbinom rexp rpois dnorm uniroot approx setNames
#' @importFrom kit topn
#' @importFrom collapse funique fmatch
#' @importFrom clock date_format
#' @importFrom fastymd fymd
#' @importFrom tibble tibble
#'
#' @examples
#' # Constant regime, 5,000 residents in 1968
#' ll <- simulate_lifelines(5000, seed = 1)
#' head(ll)
#' table(ll$EVENT_FINAL_CAUSE)
#'
#' # Dynamic scenario: rising e0, falling-then-recovering TFR, migration surge
#' ll_dyn <- simulate_lifelines(
#'   n             = 5000,
#'   start_date    = "1986-01-01",
#'   e0            = c(74.5, 81.5),
#'   tfr           = c(1.45, 1.85, 1.50),
#'   net_migration = c(1, 2, 6, 3),
#'   date_format   = "Date",
#'   seed          = 2
#' )
#' attr(ll_dyn, "parameters")
#' total_pop_on_date(ll_dyn, as.Date(sprintf("%d-01-01", seq(1990, 2025, 5))))
simulate_lifelines <- function(
  n = 10000L,
  start_date = "1968-04-02",
  end_date = "2025-12-31",
  e0 = 79,
  tfr = 1.75,
  net_migration = 3,
  emigration = 8,
  mac = 30,
  e0_sex_gap = 4,
  return_share = 0.25,
  admin_rates = c(disappear = 2e-4, found = 0.2, correction = 1e-4),
  baseline_cause = "Bopael foer 1968",
  date_format = c("iso", "Date"),
  include_sex = FALSE,
  seed = NULL
) {
  # 1. Input checks ------------------------------------------------------------
  date_format <- match.arg(date_format)
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1 || n %% 1 != 0) {
    stop("Argument 'n' must be a single positive integer.", call. = FALSE)
  }
  n <- as.integer(n)

  start_int <- .as_date_int(start_date, "start_date")
  end_int <- .as_date_int(end_date, "end_date")
  if (end_int <= start_int) {
    stop("'end_date' must be after 'start_date'.", call. = FALSE)
  }

  .check_scalar(mac, "mac", 15, 49)
  .check_scalar(e0_sex_gap, "e0_sex_gap", -20, 20)
  .check_scalar(return_share, "return_share", 0, 1)

  admin_default <- c(disappear = 2e-4, found = 0.2, correction = 1e-4)
  if (!is.numeric(admin_rates) || is.null(names(admin_rates)) ||
      !all(names(admin_rates) %in% names(admin_default)) ||
      anyNA(admin_rates) || any(admin_rates < 0)) {
    stop(
      "'admin_rates' must be a named non-negative numeric vector with names ",
      "among: 'disappear', 'found', 'correction'.",
      call. = FALSE
    )
  }
  admin_default[names(admin_rates)] <- admin_rates
  admin_rates <- admin_default

  start_levels <- .lifelines_levels$start
  final_levels <- .lifelines_levels$final
  if (!is.character(baseline_cause) || length(baseline_cause) != 1L ||
      !baseline_cause %in% start_levels) {
    stop(
      "'baseline_cause' must be one of: ",
      paste0("'", start_levels, "'", collapse = ", "),
      call. = FALSE
    )
  }
  base_code <- match(baseline_cause, start_levels)

  # 2. Annual time grid and parameter trajectories -----------------------------
  y0 <- .year_of(start_int)
  y1 <- .year_of(end_int)
  jan1 <- if (y1 > y0) .jan1_int(seq.int(y0 + 1L, y1)) else integer()
  bounds <- c(start_int, jan1, end_int + 1L)
  n_steps <- length(bounds) - 1L

  e0_k <- .expand_param(e0, n_steps, "e0", 15, 105)
  tfr_k <- .expand_param(tfr, n_steps, "tfr", 0, 15)
  nm_k <- .expand_param(net_migration, n_steps, "net_migration", -500, 500)
  em_k <- .expand_param(emigration, n_steps, "emigration", 0, 500)
  em_k <- pmax(em_k, -nm_k) # immigration can not be negative
  im_k <- em_k + nm_k

  # 3. Model schedules (ages 0..110, 110 = open interval) ----------------------
  ages <- 0:110
  mu_base <- .siler_hazard(ages + 0.5)
  rc <- .rogers_castro(ages + 0.5)
  asfr_shape <- .asfr_shape(ages, mac)
  p_male <- 1.05 / 2.05

  # Mortality multipliers per sex and year; solve only for unique targets
  e0_sex <- cbind(male = e0_k - e0_sex_gap / 2, female = e0_k + e0_sex_gap / 2)
  if (any(e0_sex < 15 | e0_sex > 105)) {
    stop("Sex-specific e0 must lie within [15, 105].", call. = FALSE)
  }
  e0_u <- collapse::funique(as.vector(e0_sex))
  theta_u <- vapply(e0_u, .solve_mortality_level, numeric(1L), mu = mu_base)
  theta <- matrix(
    theta_u[collapse::fmatch(as.vector(e0_sex), e0_u)],
    ncol = 2L
  )

  # 4. RNG handling ------------------------------------------------------------
  if (!is.null(seed)) {
    old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(
      if (is.null(old_seed)) {
        rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", old_seed, envir = globalenv())
      },
      add = TRUE
    )
    set.seed(seed)
  }

  # 5. Baseline stock from the stable population -------------------------------
  mu1 <- outer(mu_base, theta[1L, ])
  Lx1 <- .Lx(mu1[, 2L])
  nrr <- tfr_k[1L] * (1 - p_male) * sum(asfr_shape * Lx1)
  r <- log(max(nrr, 1e-3)) / mac + nm_k[1L] / 1000
  c_x <- rowMeans(apply(mu1, 2L, .Lx)) * exp(-r * (ages + 0.5))

  age0 <- sample.int(111L, n, replace = TRUE, prob = c_x) - 1L
  act <- list(
    id = seq_len(n),
    birth = start_int - as.integer((age0 + stats::runif(n)) * 365.25),
    sex = 1L + (stats::runif(n) >= p_male),
    start = rep.int(start_int, n),
    cause = rep.int(base_code, n),
    seg = rep.int(1L, n)
  )
  next_id <- n
  pool_template <- list(
    id = integer(), birth = integer(), sex = integer(),
    seg = integer(), exit = integer()
  )
  emi <- pool_template # former emigrants (candidates for return)
  lost <- pool_template # disappeared persons (candidates for "Fundet")
  out <- vector("list", 2L * n_steps + 1L)

  new_ids <- function(m) {
    ids <- next_id + seq_len(m)
    next_id <<- next_id + m
    ids
  }

  # 6. Annual simulation loop --------------------------------------------------
  for (k in seq_len(n_steps)) {
    a <- bounds[k]
    span <- bounds[k + 1L] - a
    L <- span / 365.25
    mu_k <- outer(mu_base, theta[k, ])
    f_k <- tfr_k[k] * asfr_shape

    n_act <- length(act$id)
    # completed age at mid-interval, so that age-specific rates apply on average
    # to the right exposure
    ai <- .age_index(a + span %/% 2L, act$birth)

    # Competing exits: death, emigration, disappearance, correction
    h_d <- mu_k[cbind(ai, act$sex)]
    w_e <- rc[ai]
    h_e <- w_e * (em_k[k] / 1000 * n_act / sum(w_e))
    h_cum <- cbind(
      h_d,
      h_d + h_e,
      h_d + h_e + admin_rates[["disappear"]],
      h_d + h_e + admin_rates[["disappear"]] + admin_rates[["correction"]]
    )
    h_tot <- h_cum[, 4L]
    ix <- which(stats::runif(n_act) < -expm1(-h_tot * L))
    u <- stats::runif(length(ix)) * h_tot[ix]
    hc <- h_cum[ix, , drop = FALSE]
    # 1 = Doed, 2 = Udvandret, 4 = Forsvundet, 3 = Korrektioner
    exit_type <- c(1L, 2L, 4L, 3L)[1L + (u >= hc[, 1L]) + (u >= hc[, 2L]) +
                                       (u >= hc[, 3L])]
    exit_time <- a + as.integer(stats::runif(length(ix)) * span)

    # Births (mother must still be resident on the birth date)
    t_exit <- rep.int(a + span, n_act)
    t_exit[ix] <- exit_time
    fem <- which(act$sex == 2L & ai >= 16L & ai <= 50L)
    # Poisson counts keep E[births] = f(x) * exposure exactly (no truncation)
    mom <- rep.int(fem, stats::rpois(length(fem), f_k[ai[fem]] * L))
    tb <- a + as.integer(stats::runif(length(mom)) * span)
    tb <- tb[tb <= t_exit[mom]]
    nb <- length(tb)
    nb_sex <- 1L + (stats::runif(nb) >= p_male)
    nb_id <- new_ids(nb)
    rest <- a + span - tb
    nb_dies <- stats::runif(nb) < -expm1(-mu_k[1L, nb_sex] * rest / 365.25)
    nb_death <- tb + as.integer(stats::runif(nb) * rest)

    # Close exited segments (incl. newborns dying in their birth year)
    out[[2L * k - 1L]] <- list(
      id = c(act$id[ix], nb_id[nb_dies]),
      birth = c(act$birth[ix], tb[nb_dies]),
      sex = c(act$sex[ix], nb_sex[nb_dies]),
      start = c(act$start[ix], tb[nb_dies]),
      cause = c(act$cause[ix], rep.int(7L, sum(nb_dies))),
      final = c(exit_time, nb_death[nb_dies]),
      fcause = c(exit_type, rep.int(1L, sum(nb_dies)))
    )

    # Immigration: returning emigrants + new immigrants
    lam <- im_k[k] / 1000 * n_act * L
    n_imm <- as.integer(floor(lam) + (stats::runif(1L) < lam - floor(lam)))
    ret <- integer()
    if (n_imm > 0L && length(emi$id) > 0L) {
      ai_e <- .age_index(a, emi$birth)
      w_r <- rc[ai_e] * exp(-0.1 * (a - emi$exit) / 365.25) * (ai_e <= 91L)
      n_ret <- min(stats::rbinom(1L, n_imm, return_share), sum(w_r > 0))
      if (n_ret > 0L) {
        # Weighted sampling w/o replacement (Efraimidis-Spirakis) via kit::topn
        key <- stats::rexp(length(w_r)) / w_r
        ret <- kit::topn(key, n_ret, decreasing = FALSE, index = TRUE)
      }
    }
    n_ret <- length(ret)
    n_new <- n_imm - n_ret
    t_ret <- a + as.integer(stats::runif(n_ret) * span)
    t_new <- a + as.integer(stats::runif(n_new) * span)
    age_new <- sample.int(111L, n_new, replace = TRUE, prob = rc) - 1L

    # Re-finding of disappeared persons
    fnd <- which(
      stats::runif(length(lost$id)) < -expm1(-admin_rates[["found"]] * L)
    )
    t_fnd <- a + as.integer(stats::runif(length(fnd)) * span)

    entrants <- list(
      id = c(nb_id[!nb_dies], emi$id[ret], new_ids(n_new), lost$id[fnd]),
      birth = c(
        tb[!nb_dies], emi$birth[ret],
        t_new - as.integer((age_new + stats::runif(n_new)) * 365.25),
        lost$birth[fnd]
      ),
      sex = c(
        nb_sex[!nb_dies], emi$sex[ret],
        1L + (stats::runif(n_new) >= 0.5), lost$sex[fnd]
      ),
      start = c(tb[!nb_dies], t_ret, t_new, t_fnd),
      cause = c(
        rep.int(7L, nb - sum(nb_dies)), rep.int(2L, n_ret + n_new),
        rep.int(6L, length(fnd))
      ),
      seg = c(
        rep.int(1L, nb - sum(nb_dies)), emi$seg[ret] + 1L,
        rep.int(1L, n_new), lost$seg[fnd] + 1L
      )
    )

    # Update pools: drop re-entered, add new leavers, prune the very old
    if (n_ret > 0L) emi <- .take(emi, -ret)
    if (length(fnd) > 0L) lost <- .take(lost, -fnd)
    leavers <- list(
      id = act$id[ix], birth = act$birth[ix], sex = act$sex[ix],
      seg = act$seg[ix], exit = exit_time
    )
    emi <- .bind(emi, .take(leavers, which(exit_type == 2L)))
    lost <- .bind(lost, .take(leavers, which(exit_type == 4L)))
    emi <- .take(emi, which(a - emi$birth < 95 * 365.25))
    lost <- .take(lost, which(a - lost$birth < 105 * 365.25))

    # Update active population
    if (length(ix) > 0L) act <- .take(act, -ix)
    act <- .bind(act, entrants)
  }

  # Right-censor survivors at end_date
  out[[2L * n_steps + 1L]] <- list(
    id = act$id, birth = act$birth, sex = act$sex, start = act$start,
    cause = act$cause, final = rep.int(end_int, length(act$id)),
    fcause = rep.int(5L, length(act$id))
  )

  # 7. Assemble LIFELINES schema -----------------------------------------------
  out <- out[!vapply(out, is.null, logical(1L))]
  cols <- c("id", "birth", "sex", "start", "cause", "final", "fcause")
  res <- lapply(stats::setNames(cols, cols), function(v) {
    unlist(lapply(out, `[[`, v), use.names = FALSE)
  })
  o <- order(res$id, res$start, method = "radix")
  res <- lapply(res, `[`, o)

  id_width <- max(7L, nchar(next_id))
  fmt_date <- switch(
    date_format,
    iso = .format_iso_fast,
    Date = function(x) structure(x, class = "Date")
  )

  ll <- tibble::tibble(
    PERSON_ID = sprintf("SYN_%0*d", id_width, res$id),
    BIRTHDAY = fmt_date(res$birth),
    EVENT_START_DATE = fmt_date(res$start),
    EVENT_START_CAUSE = structure(
      res$cause, levels = start_levels, class = "factor"
    ),
    EVENT_FINAL_DATE = fmt_date(res$final),
    EVENT_FINAL_CAUSE = structure(
      res$fcause, levels = final_levels, class = "factor"
    ),
    N_SEGMENTS = tabulate(res$id, next_id)[res$id]
  )
  if (isTRUE(include_sex)) ll$SEX <- res$sex

  attr(ll, "parameters") <- tibble::tibble(
    year = .year_of(bounds[-length(bounds)]),
    e0 = e0_k,
    e0_male = e0_sex[, 1L],
    e0_female = e0_sex[, 2L],
    tfr = tfr_k,
    net_migration = nm_k,
    emigration = em_k,
    immigration = im_k
  )
  ll
}


# internal helpers -------------------------------------------------------------

# Factor levels (and integer codes) shared with `codebook`
.lifelines_levels <- list(
  start = c(
    "Foedt foer 1968", "Indvandret", "Bopael", "Bopael foer 1968",
    "Indvandret foer 1968", "Fundet", "Foedt"
  ),
  final = c("Doed", "Udvandret", "Korrektioner", "Forsvundet", "I befolkningen")
)

.as_date_int <- function(x, arg) {
  if (length(x) != 1L || is.na(x)) {
    stop(sprintf("'%s' must be a single date.", arg), call. = FALSE)
  }
  d <- if (inherits(x, "Date")) x else fastymd::fymd(as.character(x))
  if (is.na(d)) {
    stop(sprintf("'%s' could not be parsed as a date.", arg), call. = FALSE)
  }
  as.integer(unclass(d))
}

.year_of <- function(int) as.POSIXlt(structure(int, class = "Date"))$year + 1900L

.jan1_int <- function(years) {
  as.integer(unclass(as.Date(sprintf("%d-01-01", years))))
}

.check_scalar <- function(x, arg, lo, hi) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < lo || x > hi) {
    stop(
      sprintf("'%s' must be a single number within [%s, %s].", arg, lo, hi),
      call. = FALSE
    )
  }
  invisible(x)
}

# scalar -> recycled; length n_steps -> as is; other -> linear interpolation
.expand_param <- function(x, n_steps, arg, lo, hi) {
  if (!is.numeric(x) || length(x) < 1L || anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("'%s' must be a non-empty finite numeric vector.", arg),
         call. = FALSE)
  }
  if (any(x < lo | x > hi)) {
    stop(sprintf("'%s' must lie within [%s, %s].", arg, lo, hi), call. = FALSE)
  }
  if (length(x) == 1L) return(rep.int(as.numeric(x), n_steps))
  if (length(x) == n_steps) return(as.numeric(x))
  if (n_steps == 1L) return(as.numeric(x[1L]))
  stats::approx(
    x = seq(0, 1, length.out = length(x)),
    y = x,
    xout = seq(0, 1, length.out = n_steps)
  )$y
}

.age_index <- function(t, birth) {
  ai <- as.integer((t - birth) / 365.25)
  ai[ai > 110L] <- 110L
  ai + 1L
}

# Siler (1979) baseline hazard; shape resembling low-mortality populations
.siler_hazard <- function(x) {
  0.02 * exp(-2.5 * x) + 3e-4 + 3e-5 * exp(0.098 * x)
}

# Person-years lived in each single-year age interval (constant hazards)
.Lx <- function(mu) {
  p <- exp(-mu)
  m <- length(mu)
  lx <- c(1, cumprod(p))[seq_len(m)]
  Lx <- lx * (1 - p) / mu
  Lx[m] <- lx[m] / mu[m]
  Lx
}

.solve_mortality_level <- function(e0, mu) {
  f <- function(log_theta) sum(.Lx(exp(log_theta) * mu)) - e0
  exp(stats::uniroot(f, c(-15, 10), tol = 1e-10)$root)
}

# Standard Rogers-Castro (1981) 7-parameter migration schedule
.rogers_castro <- function(x) {
  0.02 * exp(-0.1 * x) +
    0.06 * exp(-0.1 * (x - 20) - exp(-0.4 * (x - 20))) +
    0.003
}

# Normalised ASFR shape on ages 15..49 (sums to 1 over single-year ages)
.asfr_shape <- function(ages, mac, sd = 5.5) {
  s <- stats::dnorm(ages + 0.5, mean = mac, sd = sd) * (ages >= 15 & ages <= 49)
  s / sum(s)
}

.take <- function(lst, idx) lapply(lst, `[`, idx)

.bind <- function(a, b) Map(c, a, b)

# Format only unique dates, then broadcast -- far faster than format() on
# millions of rows
.format_iso_fast <- function(x) {
  u <- collapse::funique(x)
  clock::date_format(structure(u, class = "Date"), format = "%Y-%m-%d")[
    collapse::fmatch(x, u)
  ]
}
