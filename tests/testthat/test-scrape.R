source("../../R/scrape.R")

# ----------------------------------------------------------------------------
# parse_dates_from_text
# ----------------------------------------------------------------------------

test_that("parse_dates_from_text returns empty tibble for no-year input", {
  text <- "\nLine without any year\nAnother line\n"
  result <- parse_dates_from_text(text)
  expect_equal(nrow(result), 0)
  expect_named(result, c("entry", "year_text", "year"))
})

test_that("parse_dates_from_text extracts single year per entry", {
  text <- "\nMonument A (1875) by some group\nMonument B (1903) by another group\n"
  result <- parse_dates_from_text(text)
  expect_equal(nrow(result), 2)
  expect_equal(result$year, c(1875, 1903))
})

test_that("parse_dates_from_text splits multi-year entries into multiple rows", {
  text <- "\nMonument C with two dates (1907) and (1955)\n"
  result <- parse_dates_from_text(text)
  expect_equal(nrow(result), 2)
  expect_equal(result$year, c(1907, 1955))
  expect_equal(result$entry, rep("Monument C with two dates (1907) and (1955)", 2))
})

test_that("parse_dates_from_text filters lines containing ^ (citation noise)", {
  text <- "\nGood entry (1900)\nNoisy ^citation (1901) line\n"
  result <- parse_dates_from_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$year, 1900)
})

test_that("parse_dates_from_text ignores lines without a (YYYY) token", {
  text <- "\nNo year here\nYear in plain prose 1850 without parens\nWith parens (1900)\n"
  result <- parse_dates_from_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$year, 1900)
})

# ----------------------------------------------------------------------------
# grab_dates (against a local fixture, not the network)
# ----------------------------------------------------------------------------

test_that("grab_dates parses a local fixture file", {
  fixture_path <- normalizePath("fixtures/monuments_sample.html")
  result <- grab_dates(fixture_path)

  expect_equal(nrow(result), 5)
  expect_setequal(result$year, c(1875, 1903, 1907, 1955, 1923))
})

test_that("grab_dates writes raw HTML to cache_dir when provided", {
  fixture_path <- normalizePath("fixtures/monuments_sample.html")
  cache_dir <- tempfile("scrape_cache_")

  result <- grab_dates(fixture_path, cache_dir = cache_dir)

  expect_true(dir.exists(cache_dir))
  cached_files <- list.files(cache_dir, pattern = "\\.html$")
  expect_length(cached_files, 1)
  expect_equal(cached_files, "monuments_sample.html")

  unlink(cache_dir, recursive = TRUE)
})

# ----------------------------------------------------------------------------
# STATE_URLS constant
# ----------------------------------------------------------------------------

test_that("STATE_URLS has the expected named entries", {
  expect_setequal(
    names(STATE_URLS),
    c("alabama", "georgia", "mississippi", "north_carolina", "south_carolina", "other")
  )
  expect_true(all(grepl("^https://en\\.wikipedia\\.org/wiki/", STATE_URLS)))
})
