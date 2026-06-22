# R/scrape.R
#
# Web-scraping helpers for confederate-monument dates from Wikipedia.
# These functions are kept separate from the imperative refresh script
# (scripts/refresh_data.R) so they can be unit-tested in isolation
# (see tests/testthat/test-scrape.R) using cached HTML fixtures.

#' Source URLs to scrape for monument dates.
#'
#' Wikipedia maintains a single "List of Confederate monuments and memorials"
#' page covering most states, and dedicated per-state pages for the five
#' states with enough entries to spin off their own list.
STATE_URLS <- c(
  alabama = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_Alabama",
  georgia = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_Georgia",
  mississippi = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_Mississippi",
  north_carolina = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_North_Carolina",
  south_carolina = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_South_Carolina",
  other = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials"
)

#' Extract year-tagged monument entries from a page of plain text.
#'
#' Wikipedia's list-of-monuments pages put a year in parentheses after each
#' entry, e.g. `Fort Lee, Virginia (1917) named for CSA Gen. Robert E. Lee`.
#' This function pulls every line that contains a `(YYYY)` pattern and
#' returns one row per `(YYYY)` token found (entries can have more than one
#' year if they reference multiple installations).
#'
#' Rows containing a `^` character are filtered out: those come from citation
#' anchors at the bottom of the page rather than real entries.
#'
#' @param page_text Character scalar. The full text content of the page,
#'   typically the result of `rvest::html_text(rvest::read_html(url))`.
#' @return A tibble with columns:
#'   - `entry`: the full source line for context
#'   - `year_text`: the `(YYYY)` token
#'   - `year`: the year as numeric
parse_dates_from_text <- function(page_text) {
  lines <- unlist(stringr::str_extract_all(page_text, "(?<=\\n).*(?=\\n)"))

  candidates <- tibble::tibble(entry = lines) |>
    dplyr::filter(grepl("\\(\\d{4}\\)", .data$entry)) |>
    dplyr::filter(!grepl("\\^", .data$entry))

  if (nrow(candidates) == 0) {
    return(tibble::tibble(
      entry = character(),
      year_text = character(),
      year = numeric()
    ))
  }

  candidates |>
    dplyr::rowwise() |>
    dplyr::mutate(
      year_text = paste(
        unlist(stringr::str_extract_all(.data$entry, "\\(\\d{4}\\)")),
        collapse = ", "
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::separate_rows("year_text", sep = ", ") |>
    dplyr::mutate(year = as.numeric(stringr::str_remove_all(.data$year_text, "\\(|\\)")))
}

#' Fetch a Wikipedia page and parse out monument dates.
#'
#' Network IO. For unit tests, use `parse_dates_from_text()` directly on a
#' cached page text. For end-to-end runs, this function fetches the URL and
#' optionally caches the raw HTML to disk for later reproducibility.
#'
#' @param url Page URL.
#' @param cache_dir Optional directory to write the raw HTML to. The filename
#'   is the URL's basename with a `.html` suffix. If `NULL` (default), no
#'   caching is performed.
#' @return Same tibble as `parse_dates_from_text()`.
grab_dates <- function(url, cache_dir = NULL) {
  page <- rvest::read_html(url)
  page_text <- rvest::html_text(page)

  if (!is.null(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- paste0(tools::file_path_sans_ext(basename(url)), ".html")
    writeLines(as.character(page), file.path(cache_dir, fname))
  }

  parse_dates_from_text(page_text)
}

#' Scrape all sources and return one combined tibble.
#'
#' @param urls Named character vector of source URLs. Defaults to `STATE_URLS`.
#' @param cache_dir Optional directory for raw HTML cache (passed to `grab_dates`).
#' @return A tibble with columns `entry`, `year_text`, `year`, `source`
#'   (the name from `urls`).
scrape_all <- function(urls = STATE_URLS, cache_dir = NULL) {
  results <- lapply(seq_along(urls), function(i) {
    src <- names(urls)[[i]]
    url <- urls[[i]]
    message("Scraping ", src, " (", url, ")")
    grab_dates(url, cache_dir = cache_dir) |>
      dplyr::mutate(source = src)
  })
  dplyr::bind_rows(results)
}
