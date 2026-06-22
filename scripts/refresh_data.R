#!/usr/bin/env Rscript
# scripts/refresh_data.R
#
# Re-scrapes the source Wikipedia pages and writes the combined dataset to
# data/confederate_statue_dates.csv. Intended to be run on a cadence (via
# .github/workflows/refresh-data.yml) and to also work locally:
#
#   Rscript scripts/refresh_data.R
#
# Optionally caches raw HTML to data/raw_html/ for reproducibility — pass
# --cache to enable, omit to disable. The cache is gitignored.

suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tibble)
})

repo_root <- if (rlang::is_installed("here")) here::here() else getwd()
source(file.path(repo_root, "R", "scrape.R"))

args <- commandArgs(trailingOnly = TRUE)
use_cache <- "--cache" %in% args
cache_dir <- if (use_cache) file.path(repo_root, "data", "raw_html") else NULL

out_path <- file.path(repo_root, "data", "confederate_statue_dates.csv")

message("Scraping ", length(STATE_URLS), " source pages...")
result <- scrape_all(STATE_URLS, cache_dir = cache_dir)

message("Got ", nrow(result), " rows. Writing to ", out_path)
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write.csv(result, out_path, row.names = FALSE)

message("Done.")
