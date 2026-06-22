#!/usr/bin/env Rscript
# scripts/refresh_data.R
#
# Re-runs the scrape -> clean pipeline against the checked-in raw HTML
# snapshots in `data-raw/` and writes the cleaned dataset to
# `data/confederate_statue_dates.csv`.
#
# By default this is offline-reproducible: it never hits the network.
# To refresh the raw HTML snapshots from Wikipedia first, run
# `scripts/refetch_raw.R` (called automatically by the
# `.github/workflows/refresh-data.yml` scheduled job).
#
# Usage:
#   Rscript scripts/refresh_data.R

suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tibble)
})

repo_root <- if (rlang::is_installed("here")) here::here() else getwd()
source(file.path(repo_root, "R", "scrape.R"))
source(file.path(repo_root, "R", "clean.R"))

raw_dir <- file.path(repo_root, "data-raw")
out_path <- file.path(repo_root, "data", "confederate_statue_dates.csv")

raw_files <- list.files(raw_dir, pattern = "\\.html$", full.names = TRUE)
if (length(raw_files) == 0) {
  stop(
    "No HTML snapshots found in ", raw_dir, ". ",
    "Run scripts/refetch_raw.R first to populate it from Wikipedia."
  )
}

local_urls <- setNames(
  raw_files,
  tools::file_path_sans_ext(basename(raw_files))
)

message("Parsing ", length(local_urls), " snapshots from ", raw_dir)
raw <- scrape_all(local_urls)

message("Got ", nrow(raw), " raw rows; cleaning")
clean <- tidy_statues(raw)

message(
  "Cleaned to ", nrow(clean), " rows across ",
  length(unique(clean$source)), " sources. Writing to ", out_path
)
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write.csv(clean, out_path, row.names = FALSE)

message("Done.")
