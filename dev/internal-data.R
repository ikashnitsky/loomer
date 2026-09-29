# ..........................................................
# 2026-09-29 -- loomer
# internal data                -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................


# save codelists into the package
library(tidyverse)
library(nanoparquet)

codebook <- read_parquet("K:/Lifelines/prq/lifelines_codebook.parquet")

codebook |> usethis::use_data()
