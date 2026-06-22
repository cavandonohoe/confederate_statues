#!/usr/bin/env Rscript
# scripts/refetch_raw.R
#
# Re-downloads the Wikipedia source pages and overwrites the HTML
# snapshots in `data-raw/`. Run this before `refresh_data.R` if you
# want the dataset to reflect the latest upstream content; otherwise
# `refresh_data.R` uses whatever is already in `data-raw/` (offline-
# reproducible).
#
# Run by the scheduled workflow at `.github/workflows/refresh-data.yml`.
#
# Usage:
#   Rscript scripts/refetch_raw.R

suppressPackageStartupMessages({
  library(rvest)
})

repo_root <- if (rlang::is_installed("here")) here::here() else getwd()
source(file.path(repo_root, "R", "scrape.R"))

raw_dir <- file.path(repo_root, "data-raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

for (i in seq_along(STATE_URLS)) {
  src <- names(STATE_URLS)[i]
  url <- STATE_URLS[i]
  out <- file.path(raw_dir, paste0(src, ".html"))
  message("Downloading ", src, " -> ", out)
  page <- rvest::read_html(url)
  writeLines(as.character(page), out)
}

message("Done. Run scripts/refresh_data.R next to regenerate the CSV.")
