# Basketball-Reference: season advanced tables (age, BPM for checking ours),
# player index pages (height, weight, birth date, first season) and draft pages.
# Pages are cached in data/raw/bbref. One request every 4 seconds.

library(data.table)
library(rvest)
library(xml2)

dir.create("data/raw/bbref", recursive = TRUE, showWarnings = FALSE)

fetch <- function(url, cache) {
  if (file.exists(cache)) return(read_html(cache))
  pg <- read_html(url)
  write_html(pg, cache)
  Sys.sleep(4)
  pg
}

cells <- function(rows, stat) {
  vapply(rows, function(r) {
    n <- html_element(r, sprintf("[data-stat='%s']", stat))
    if (inherits(n, "xml_missing")) NA_character_ else html_text(n)
  }, "")
}

player_ids <- function(rows, stat) {
  vapply(rows, function(r) {
    n <- html_element(r, sprintf("[data-stat='%s']", stat))
    if (inherits(n, "xml_missing")) return(NA_character_)
    id <- html_attr(n, "data-append-csv")
    if (is.na(id)) {
      a <- html_element(n, "a")
      if (!inherits(a, "xml_missing")) id <- sub(".*/players/./([^.]+)\\.html.*", "\\1", html_attr(a, "href"))
    }
    id
  }, "")
}

# season advanced tables
adv <- rbindlist(lapply(2002:2026, function(y) {
  pg <- fetch(sprintf("https://www.basketball-reference.com/leagues/NBA_%d_advanced.html", y),
              sprintf("data/raw/bbref/advanced_%d.html", y))
  tb <- html_element(pg, "table#advanced")
  rows <- html_elements(tb, "tbody tr:not(.thead)")
  name_col <- if (length(html_elements(tb, "[data-stat='name_display']"))) "name_display" else "player"
  team_col <- if (length(html_elements(tb, "[data-stat='team_name_abbr']"))) "team_name_abbr" else "team_id"
  data.table(season = y, bbref_id = player_ids(rows, name_col), player = cells(rows, name_col),
             team = cells(rows, team_col), age = as.numeric(cells(rows, "age")),
             mp = as.numeric(cells(rows, "mp")), obpm = as.numeric(cells(rows, "obpm")),
             dbpm = as.numeric(cells(rows, "dbpm")), bpm = as.numeric(cells(rows, "bpm")))
}))
adv <- adv[!is.na(bbref_id) & player != ""]
saveRDS(adv, "data/bbref_advanced.rds")

# player bios
bio <- rbindlist(lapply(letters, function(l) {
  pg <- fetch(sprintf("https://www.basketball-reference.com/players/%s/", l),
              sprintf("data/raw/bbref/players_%s.html", l))
  rows <- html_elements(pg, "table#players tbody tr:not(.thead)")
  data.table(bbref_id = player_ids(rows, "player"), player = gsub("\\*", "", cells(rows, "player")),
             year_min = as.integer(cells(rows, "year_min")), year_max = as.integer(cells(rows, "year_max")),
             height = cells(rows, "height"), weight = as.numeric(cells(rows, "weight")),
             birth_date = as.Date(cells(rows, "birth_date"), format = "%B %d, %Y"),
             colleges = cells(rows, "colleges"))
}))
bio[, height_in := vapply(strsplit(height, "-"), function(z)
  if (length(z) == 2) as.numeric(z[1]) * 12 + as.numeric(z[2]) else NA_real_, 0)]
bio[, college := !is.na(colleges) & trimws(colleges) != ""]
saveRDS(bio[year_max >= 2002], "data/bbref_bio.rds")

# draft picks
draft <- rbindlist(lapply(1984:2026, function(y) {
  pg <- fetch(sprintf("https://www.basketball-reference.com/draft/NBA_%d.html", y),
              sprintf("data/raw/bbref/draft_%d.html", y))
  rows <- html_elements(pg, "table#stats tbody tr:not(.thead)")
  d <- data.table(draft_year = y, bbref_id = player_ids(rows, "player"),
                  pick = as.integer(cells(rows, "pick_overall")))
  d[!is.na(bbref_id) & !is.na(pick)]
}))
saveRDS(draft, "data/bbref_draft.rds")
