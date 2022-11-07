
library(rvest)

url = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials"

wiki_confederate_statues = url %>% read_html()

wiki_text <- wiki_confederate_statues %>% html_nodes("#bodyContent") %>% 
  html_text()

wiki_text %>%
  # stringr::str_split("\n") %>%
  str_extract_all("\\([^()]+\\d\\d\\d\\d\\)")

wiki_text %>%
  # stringr::str_split("\n") %>%
  str_extract_all("\\(\\d\\d\\d\\d[^()]+\\)")

# this works
wiki_text %>%
  # stringr::str_split("\n") %>%
  str_extract_all("\\(\\d{4}\\)")

# (?<=\\().*(?=\\))
# Break the pattern in three parts:
# 
# (?<=\\()
# .*
# (?=\\))
# The second part is straightforward. It matches zero or more number of any character in between the opening and closing brackets.
# 
# The first part (and the third part) uses positive look behind (and positive look ahead). What they does is that it checks whether the pattern after ?<= (and ?=) and with a pair of parentheses is present or not. If present, it returns the part that matches the pattern given after (and before) this, but not the part that matches itself.
# 
# So, basically:
#   
# first part checks whether it starts with an opening bracket
# second part matched any part within opening and closing bracket
# third part checks whether it ends with a closing bracket
# I'm not really very experienced with regular expressions, so it can be done more easily and my solution and/or explanation may not be accurate in all possible scenarios. You can check this site. (https://www.regular-expressions.info/lookaround.html)
## from https://community.rstudio.com/t/extract-text-between-brakets/43448/6

grab_dates <- function(url) {
  url %>%
    read_html() %>%
    html_text() %>%
    str_extract_all("(?<=\n).*(?=\\n)") %>%
    unlist %>% as_tibble() %>%
    mutate(has_date = grepl(value, pattern = "\\(\\d{4}\\)")) %>% 
    filter(has_date) %>%
    rowwise() %>%
    mutate(value2 = str_extract_all(value, "\\(\\d{4}\\)") %>% unlist %>% paste(collapse = ", ")) %>%
    separate_rows(value2, sep = ", ") %>%
    mutate(value3 = as.numeric(stringr::str_remove_all(string = value2, pattern = "\\(|\\)")))
}

wiki_text_separated <- wiki_text %>%
  # stringr::str_split("\n") %>%
  str_extract_all("(?<=\n).*(?=\\n)") %>%
  unlist %>% as_tibble()

wiki_clean <- wiki_text_separated %>%
  mutate(has_date = grepl(value, pattern = "\\(\\d{4}\\)")) %>% 
  filter(has_date) %>%
  # get rid of this citation
  filter(value != "^ Ark. Code Ann. (1987), Section 1–4–101; cited in B.F. Shearer and B.S. Shearer (2002), State Names, Seals, Flags, and Symbols, Greenwood Press, p. 54") %>%
  rowwise() %>%
  mutate(value2 = str_extract_all(value, "\\(\\d{4}\\)") %>% unlist %>% paste(collapse = ", ")) %>%
  separate_rows(value2, sep = ", ") %>%
  mutate(value3 = as.numeric(stringr::str_remove_all(string = value2, pattern = "\\(|\\)")))

wiki_clean %>%
  ggplot(aes(x = value3)) +
  geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks = seq(min(wiki_clean$value3), max(wiki_clean$value3), by = 5))


# omg these fucking states have their own wikis for how many statues they have:
# Alabama, Georgia, Mississippi, North Carolina, South Carolina
# bama = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_Alabama"
# georgia = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_Georgia"
# mississippi = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_Mississippi"
# north carolina = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_North_Carolina"
# south carolina = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_South_Carolina"

url_bama = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_Alabama"
dates_bama = url_bama %>% grab_dates()

# ugh georgia doesnt follow the parentheses rule
# so look for any 4 digit code (year)
# lol i ended up editing the wiki
url_georgia = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_Georgia"
dates_georgia = url_georgia %>% grab_dates()

url_mississippi = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_Mississippi"
dates_mississippi = url_mississippi %>% grab_dates()

url_north_carolina = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_North_Carolina"
dates_north_carolina = url_north_carolina %>% grab_dates()

url_south_carolina = "https://en.wikipedia.org/wiki/List_of_Confederate_monuments_and_memorials_in_South_Carolina"
dates_south_carolina = url_south_carolina %>% grab_dates()

all_dates <- wiki_clean %>%
  bind_rows(dates_bama, dates_georgia, dates_mississippi, dates_north_carolina, dates_south_carolina)


all_dates %>% 
  ggplot(aes(x = value3)) +
  geom_histogram(binwidth = 5, fill = "red") +
  scale_x_continuous(breaks = seq(min(wiki_clean$value3), max(wiki_clean$value3), by = 5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=15, angle = 90),
        axis.text.y = element_text(size=20),
        title = element_text(size=20)) +
  ylab("# of Statues") +
  ggtitle("Number of Confederate Statues Over Time") #+
  # theme_bw()
