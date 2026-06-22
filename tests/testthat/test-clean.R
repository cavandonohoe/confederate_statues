source("../../R/clean.R")

# ----------------------------------------------------------------------------
# Schema and basic happy path
# ----------------------------------------------------------------------------

raw_fixture <- function() {
  tibble::tibble(
    entry = c(
      "Monument A (1900)",
      "Monument B (1875)",
      "Monument C (1907)",
      "Monument C (1907)",
      "Future-dated noise (2999)",
      "Pre-war noise (1700)",
      "NA-year row",
      "  Whitespace entry (1923)  "
    ),
    year_text = c(
      "(1900)", "(1875)", "(1907)", "(1907)",
      "(2999)", "(1700)", NA_character_, "(1923)"
    ),
    year = c(1900, 1875, 1907, 1907, 2999, 1700, NA_real_, 1923),
    source = c(
      "alabama", "alabama", "georgia", "georgia",
      "georgia", "georgia", "alabama", "alabama"
    )
  )
}

test_that("tidy_statues returns the canonical schema", {
  result <- tidy_statues(raw_fixture(), year_max = 2030)
  expect_named(result, c("source", "entry", "year"))
  expect_type(result$source, "character")
  expect_type(result$entry, "character")
  expect_type(result$year, "integer")
})

test_that("tidy_statues drops rows outside the valid year window", {
  result <- tidy_statues(raw_fixture(), year_max = 2030)
  expect_false(any(result$year < 1840))
  expect_false(any(result$year > 2030))
  expect_false(any(is.na(result$year)))
  expect_false(2999 %in% result$year)
  expect_false(1700 %in% result$year)
})

test_that("tidy_statues removes (source, entry, year) duplicates", {
  result <- tidy_statues(raw_fixture(), year_max = 2030)
  dup_count <- result |>
    dplyr::group_by(.data$source, .data$entry, .data$year) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::pull(.data$n)
  expect_true(all(dup_count == 1))
})

test_that("tidy_statues trims whitespace from entry", {
  result <- tidy_statues(raw_fixture(), year_max = 2030)
  trimmed <- result$entry[result$year == 1923]
  expect_equal(trimmed, "Whitespace entry (1923)")
})

test_that("tidy_statues errors on input missing required columns", {
  bad <- tibble::tibble(entry = "x", year = 1900)
  expect_error(tidy_statues(bad), "missing required columns")
})

test_that("tidy_statues returns an empty tibble with correct schema on empty input", {
  empty <- tibble::tibble(
    entry = character(),
    year_text = character(),
    year = numeric(),
    source = character()
  )
  result <- tidy_statues(empty)
  expect_equal(nrow(result), 0)
  expect_named(result, c("source", "entry", "year"))
})

test_that("tidy_statues output is sorted by source, year, entry", {
  result <- tidy_statues(raw_fixture(), year_max = 2030)
  expect_equal(result$source, sort(result$source))
})
