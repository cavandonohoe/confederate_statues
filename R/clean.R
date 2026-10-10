# R/clean.R
#
# Cleaning layer for the confederate-monuments pipeline.
#
# `scrape.R` returns one row per `(YYYY)` token found in a Wikipedia
# entry — including the occasional plausible-but-spurious year that
# leaks through the regex (e.g. an "as of (2024)" reference inside an
# entry's prose). `tidy_statues()` normalizes that output into the
# canonical analysis schema used by `plots.Rmd` and the README chart.
#
# The split exists so the schema is locked in by tests
# (`tests/testthat/test-clean.R`) independently of the scraping
# transport layer.

#' Valid year range for a confederate monument installation.
#'
#' Lower bound is set just before the U.S. Civil War to capture a small
#' number of legitimate entries — antebellum counties later co-opted by
#' Confederate naming (e.g. "Levy County (1845)"). Upper bound is the
#' current year. Years outside this window are either implausibly old
#' or future (almost certainly an "as of (YYYY)" reference rather than
#' an installation date).
STATUE_YEAR_MIN <- 1840
statue_year_max <- function() as.integer(format(Sys.Date(), "%Y"))

#' Tidy a raw scrape into the canonical monument dataset.
#'
#' @param raw A tibble as returned by `scrape_all()`, with columns
#'   `entry`, `year_text`, `year`, and `source`.
#' @param year_min Lower-bound year filter (default `STATUE_YEAR_MIN`).
#' @param year_max Upper-bound year filter (default current year).
#' @return A tibble with columns:
#'   - `source`: character. Which Wikipedia page the row came from.
#'   - `entry`: character. The full source line for context.
#'   - `year`: integer. The installation year.
#'   One row per (source, entry, year) — duplicates removed.
tidy_statues <- function(raw,
                         year_min = STATUE_YEAR_MIN,
                         year_max = statue_year_max()) {
  required <- c("entry", "year_text", "year", "source")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop(
      "tidy_statues(): input is missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }

  raw |>
    dplyr::filter(
      !is.na(.data$year),
      .data$year >= year_min,
      .data$year <= year_max
    ) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      entry = trimws(.data$entry)
    ) |>
    dplyr::distinct(.data$source, .data$entry, .data$year) |>
    dplyr::select("source", "entry", "year") |>
    dplyr::arrange(.data$source, .data$year, .data$entry)
}

#' Default shrink-guard thresholds for complete snapshot refreshes.
#'
#' A refresh replaces the whole dataset (and the service ingest deletes rows
#' absent from the snapshot), so a truncated-but-well-formed parse would
#' silently delete data. These mirror `MAX_TOTAL_SHRINK` /
#' `MAX_SOURCE_SHRINK` in `service/src/statue_api/ingest.py`.
STATUE_MAX_TOTAL_SHRINK <- 0.2
STATUE_MAX_SOURCE_SHRINK <- 0.5

#' Compare a new snapshot against the current dataset and report shrinkage.
#'
#' @param new,current Data frames with a `source` column (one row per entry).
#' @param max_total_shrink Largest allowed fractional drop in total rows.
#' @param max_source_shrink Largest allowed fractional drop for any source
#'   present in `current` (a source vanishing entirely always counts).
#' @return A character vector of problems; empty when the snapshot is OK.
check_snapshot_shrink <- function(new,
                                  current,
                                  max_total_shrink = STATUE_MAX_TOTAL_SHRINK,
                                  max_source_shrink = STATUE_MAX_SOURCE_SHRINK) {
  problems <- character()
  n_current <- nrow(current)
  n_new <- nrow(new)
  if (n_current > 0 && n_new < n_current * (1 - max_total_shrink)) {
    problems <- c(problems, sprintf(
      "total rows would drop from %d to %d (more than %.0f%%)",
      n_current, n_new, 100 * max_total_shrink
    ))
  }
  current_counts <- table(current$source)
  new_counts <- table(factor(new$source, levels = names(current_counts)))
  for (src in sort(names(current_counts))) {
    before <- as.integer(current_counts[[src]])
    after <- as.integer(new_counts[[src]])
    if (before > 0 && after < before * (1 - max_source_shrink)) {
      problems <- c(problems, sprintf(
        "source '%s' would drop from %d to %d (more than %.0f%%)",
        src, before, after, 100 * max_source_shrink
      ))
    }
  }
  problems
}

#' Whether the shrink guard has been explicitly overridden.
#'
#' True when `--force` is passed on the command line or
#' `STATUE_ALLOW_SHRINK` is set to a truthy value.
shrink_override_requested <- function(args = commandArgs(trailingOnly = TRUE)) {
  "--force" %in% args ||
    tolower(Sys.getenv("STATUE_ALLOW_SHRINK", "")) %in% c("1", "true", "yes")
}
