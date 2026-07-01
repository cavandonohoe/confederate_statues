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

test_that("parse_dates_from_text drops up-arrow citation back-links", {
  text <- paste0(
    "\nReal monument (1900)\n",
    "\u2191 Domby, Adam (2017). \"Captives of Memory\". Civil War History.\n",
    "\u2191 \"Photograph of a statue (1930)\". Umbra Search. Retrieved 2017.\n"
  )
  result <- parse_dates_from_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$year, 1900)
})

test_that("parse_dates_from_text drops numbered back-reference citations", {
  text <- paste0(
    "\nReal monument (1899)\n",
    "1 2 3 4 Wiggins, David N. (2005). Remembering Georgia's Confederates.\n",
    "1929 Confederate Reunion Marker (1929). Erected by citizens.\n"
  )
  result <- parse_dates_from_text(text)
  # The citation is dropped; the real 1929 marker (single leading year) is kept.
  expect_equal(nrow(result), 2)
  expect_setequal(result$year, c(1899, 1929))
})

test_that("parse_dates_from_text strips the .mw-parser-output CSS block", {
  # A pure citation-CSS line whose only (YYYY) is inside the CSS drops out...
  css_only <- paste0(
    "1 2 3 .mw-parser-output cite.citation{font-style:inherit} ",
    "some cited work (2016)"
  )
  # ...but a real entry that merely has the CSS appended keeps its year.
  real_plus_css <- paste0(
    "Eternal Flame Monument (1939), plaque reads: ",
    ".mw-parser-output .templatequote{overflow:hidden} lit in 2016"
  )
  text <- paste0("\n", css_only, "\n", real_plus_css, "\n")
  result <- parse_dates_from_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$year, 1939)
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

test_that("grab_dates parses the Virginia fixture in the state-page style", {
  fixture_path <- normalizePath("fixtures/virginia_sample.html")
  result <- grab_dates(fixture_path)

  expect_equal(nrow(result), 6)
  expect_setequal(result$year, c(1910, 1902, 1893, 1905, 1903, 1937))
})

# ----------------------------------------------------------------------------
# STATE_URLS constant
# ----------------------------------------------------------------------------

test_that("STATE_URLS has the expected named entries", {
  expect_setequal(
    names(STATE_URLS),
    c(
      "alabama", "georgia", "mississippi", "north_carolina",
      "south_carolina", "virginia", "other"
    )
  )
  expect_true(all(grepl("^https://en\\.wikipedia\\.org/wiki/", STATE_URLS)))
})
