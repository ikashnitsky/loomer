

# **loomer: A high-performance auxiliary R package for analyzing the Danish Lifelines dataset** <a href='[https://github.com/ikashnitsky/loomer](https://github.com/ikashnitsky/loomer)'><img src='inst/figures/logo-192.png' align="right" width="20%" min-width="200px"/></a>

<!-- badges: start -->

[![Version-Number](https://img.shields.io/github/r-package/v/ikashnitsky/loomer?label=loomer&logo=R&style=flat.png)](https://github.com/ikashnitsky/loomer)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/loomer)](https://CRAN.R-project.org/package=loomer) -->
<!-- [![Contributors](https://img.shields.io/github/contributors/ikashnitsky/loomer?style=flat)](https://github.com/ikashnitsky/loomer/graphs/contributors) -->
![Bluesky
followers](https://img.shields.io/bluesky/followers/:ikashnitsky.phd)
<!-- badges: end -->

The goal of **loomer** is to provide high-performance, reproducible
workflows for constructing, slicing, and analyzing longitudinal
population register data (Lifelines) in R. Built specifically to handle
multi-million-row demographic registers, **loomer** pairs modern
tidyverse interfaces with C-backed fastverse engines (`{collapse}`,
`{kit}`, `{stringfish}`, and `{fastymd}`) to streamline date parsing,
categorical codebook decoding, fast population point-in-time querying,
and cohort extraction without memory bottlenecks.

## Installation

You can install `loomer` from github:

``` r
pak::pak("ikashnitsky/loomer")
```

## Usage & Examples

``` r
library(loomer)
library(dplyr)
library(clock)

# Load the packaged synthetic lifelines benchmark dataset (10,000 records)
data(synthetic_lifelines, package = "loomer")

# Filter active residents on a specific census date
active_at_census <- in_population_on_date(synthetic_lifelines, "2020-01-01")
nrow(active_at_census)

# Construct a closed cohort: age 20-60 on 2010-01-01 with >= 1 year follow-up
cohort <- construct_cohort(
  lifelines_dataset = synthetic_lifelines,
  age               = c(20, 60),
  date              = as.Date(c("2010-01-01", "2020-12-31")),
  open              = FALSE,
  pre_entry_y       = 0,
  post_entry_y      = 1.0,
  multi_lines       = FALSE,
  plot              = FALSE,
  verbose           = FALSE
)

cohort |> glimpse()
```

## Lifelines dataset

The **Lifelines** project was developed at **Danmarks Statistik
(Statistics Denmark)**. Its core purpose is to construct a comprehensive
Danish longitudinal event dataset that tracks individuals’ life events
(such as birth, immigration, emigration, and death) across time by
integrating multiple national administrative registries (including CPR
register data, population statistics, migration records, and fertility
databases).

**Authors & Contributors:** \* **Ilya Kashnitsky**: Main developer and
maintainer of the project from 2024. \* **Markus Klink Damgaard**:
Designed and implemented the underlying object-oriented rule system and
rule execution framework used to validate and filter complex
chronological event sequences. \* **Anette Nymand Rasmussen**: SAS
pipeline developer and maintainer in 2023-24, historical CPR number/ID
shift resolution, population statistics validation, and ongoing dataset
production runs. \* **Additional Contributors**: Development logs and
codebase records note additional contributions and analytical support
from team members including **Jolien Cremers**, **Linnea Lanzky**, and
**Frederik Christensen**.
