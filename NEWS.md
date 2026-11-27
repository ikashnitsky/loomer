# loomer (development version)

* New `simulate_lifelines()` generates synthetic LIFELINES populations of any
  size via a vectorised annual microsimulation, with constant or time-varying
  `e0`, `tfr`, and net migration rates.

* `in_population_on_date()` and `total_pop_on_date()` now work with ISO
  `"YYYY-MM-DD"` character date columns (as in `synthetic_lifelines`).
  Previously these were compared lexicographically and silently returned
  zero rows. `POSIXct` columns are supported too; other types give an error.

* `from_yyyymmdd_iso_txt()` no longer fails on data with factor columns, and
  auto-detection now only converts character columns whose first value matches
  `"YYYY-MM-DD"` (previously any 10-character string was parsed, giving `NA`s).

* Removed duplicated definitions of `to_yyyymmdd_iso_txt()` and
  `from_yyyymmdd_iso_txt()` from `R/encode-decode.R`.

# loomer 0.2.0

* Added a `NEWS.md` file to track changes to the package.
