#!/usr/bin/env Rscript
# scripts/validate_data.R
#
# Sanity-checks the regenerated dataset in
# `data/confederate_statue_dates.csv` against the canonical schema and a
# set of guardrails. Intended to run in CI immediately after
# `scripts/refresh_data.R`, before a refresh PR is opened, so a broken
# Wikipedia parse can never silently land as a green "no checks" PR.
#
# Exits non-zero (with a human-readable message) on the first failed
# guardrail. Usage:
#   Rscript scripts/validate_data.R

suppressPackageStartupMessages({
  library(dplyr)
})

repo_root <- if (rlang::is_installed("here")) here::here() else getwd()
source(file.path(repo_root, "R", "clean.R"))

csv_path <- file.path(repo_root, "data", "confederate_statue_dates.csv")

fail <- function(...) {
  message("VALIDATION FAILED: ", ...)
  quit(status = 1)
}

if (!file.exists(csv_path)) {
  fail("dataset not found at ", csv_path)
}

data <- utils::read.csv(csv_path, stringsAsFactors = FALSE)

# 1. Schema: exact columns in the canonical order.
expected_cols <- c("source", "entry", "year")
if (!identical(names(data), expected_cols)) {
  fail(
    "unexpected columns. Expected [",
    paste(expected_cols, collapse = ", "),
    "], got [", paste(names(data), collapse = ", "), "]"
  )
}

# 2. Non-empty. An empty dataset almost always means the parser broke
#    (e.g. Wikipedia changed its markup and no (YYYY) tokens matched).
if (nrow(data) == 0) {
  fail("dataset has zero rows; the scrape/clean pipeline likely broke")
}

# 3. year: integer-valued, no NAs, inside the valid window.
year_max <- statue_year_max()
if (anyNA(data$year)) {
  fail(sum(is.na(data$year)), " row(s) have a missing year")
}
if (!all(data$year == as.integer(data$year))) {
  fail("year column contains non-integer values")
}
out_of_range <- data$year < STATUE_YEAR_MIN | data$year > year_max
if (any(out_of_range)) {
  fail(
    sum(out_of_range), " row(s) fall outside the valid year window [",
    STATUE_YEAR_MIN, ", ", year_max, "]"
  )
}

# 4. entry / source: no missing or blank values.
if (anyNA(data$entry) || any(!nzchar(trimws(data$entry)))) {
  fail("one or more rows have a missing or blank entry")
}
if (anyNA(data$source) || any(!nzchar(trimws(data$source)))) {
  fail("one or more rows have a missing or blank source")
}

# 5. No (source, entry, year) duplicates should survive tidy_statues().
dup <- data |>
  dplyr::count(.data$source, .data$entry, .data$year) |>
  dplyr::filter(.data$n > 1)
if (nrow(dup) > 0) {
  fail(nrow(dup), " duplicate (source, entry, year) row(s) present")
}

# 6. Every known state source should still contribute rows. If a source
#    drops to zero, that page's parse silently failed even though the
#    overall dataset is non-empty.
expected_sources <- c(
  "alabama", "georgia", "mississippi", "north_carolina",
  "other", "south_carolina", "virginia"
)
present <- unique(data$source)
missing_sources <- setdiff(expected_sources, present)
if (length(missing_sources) > 0) {
  fail(
    "expected source(s) contribute no rows: ",
    paste(missing_sources, collapse = ", ")
  )
}

message(
  "Validation OK: ", nrow(data), " rows across ",
  length(present), " sources; years in [",
  min(data$year), ", ", max(data$year), "]."
)
