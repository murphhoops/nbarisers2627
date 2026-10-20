# Fill the board template with the three lists and write docs/index.html.

library(data.table)
library(jsonlite)

lists <- readRDS("data/board_lists.rds")
bt <- readRDS("data/backtest.rds")

rows <- function(x, type) {
  lapply(seq_len(nrow(x)), function(i) {
    r <- x[i]
    list(
      name = r$player, exp = r$exp_years, age = r$age,
      last = r$team_last, `next` = ifelse(is.na(r$team_next), "", r$team_next), status = r$status,
      now = round(r$value), proj = round(r$proj), lo = round(r$proj_lo), hi = round(r$proj_hi),
      p_main = round(100 * switch(type, breakout = r$p_breakout, diamond = r$p_60, fall = r$p_fall)),
      p_role = round(100 * r$p_role), p_top = round(100 * r$p_top_quarter),
      p_if_role = round(100 * r$p_d60), p_off = round(100 * r$p_off),
      bpm = round(r$bpm, 1), multi = round(r$multi_est, 1),
      chg1 = if (is.na(r$value_chg1) || r$exp_years == 0) NULL else round(r$value_chg1),
      mp = round(r$mp), g = r$g,
      parts = list(scoring = round(r$scoring, 1), playmaking = round(r$playmaking, 1),
                   rebounding = round(r$rebounding, 1), defense = round(r$defense_box, 1),
                   team = round(r$team_context, 1)))
  })
}

pct <- function(x) round(100 * x)
teams_note <- "Team moves come from ESPN's 2026-27 rosters; a team in orange means he changed teams."

data <- list(
  breakout = list(
    threshold = 75, thr_label = "Top-quarter line (75)", pill_label = "Breakout %",
    sub = paste("The 25 players with the best chance of a top-quarter season in 2026-27 (score 75 or higher).",
                "Players with four or fewer years of experience and 300+ minutes in 2025-26 who are currently at or below 75."),
    foot = paste(
      "Breakout % is the chance he finishes at 75 or higher, given a real role (1,000+ minutes). The arrow and its range",
      "assume the same real role, so every number on a row describes the same season. The small line under the percentage",
      sprintf("multiplies in his chance of getting that role. In the 2016-2025 backtest, %d of the 100 top-ten picks reached 75 with a real role,",
              bt$breakout$top10_hits),
      sprintf("against %d%% of the pool, and %d of the %d who earned a real role got there.",
              pct(bt$breakout$pool_rate), bt$breakout$top10_hits, bt$breakout$top10_role),
      "Chance of a real role reads a little high between 30% and 50%.", teams_note),
    rows = rows(lists$breakout, "breakout")),
  diamond = list(
    threshold = 60, thr_label = "Useful-starter line (60)", pill_label = "Chance of 60+",
    sub = paste("The 25 players 27 or younger, currently below 50, with the best chance of a 60+ season with a real role",
                "in 2026-27. Every one of them has to climb at least 10 points to get there."),
    foot = paste(
      "Chance of 60+ is the chance he plays a real role (1,000+ minutes) and scores 60 or higher; the small line is the same",
      "chance if he does get the role, and the arrow is his projected score in that case.",
      sprintf("In the 2016-2025 backtest, %d%% of each year's top 25 did it, against %d%% of the rest of the pool, and %d%% of those",
              pct(bt$diamond$top25_rate), pct(bt$diamond$rest_rate), pct(bt$diamond$top25_reach75)),
      "who played 500+ minutes reached 75. The projected score alone overrates the lowest scorers, so this list ranks by",
      "the chance of becoming useful, not by the size of the climb.", teams_note),
    rows = rows(lists$diamond, "diamond")),
  fall = list(
    threshold = 60, thr_label = "Useful-starter line (60)", pill_label = "Chance of falling",
    sub = paste("The 25 top-quarter players (75 or higher, 1,000+ minutes in 2025-26) most likely to fall below 60 in 2026-27",
                "if they keep a real role."),
    foot = paste(
      "Chance of falling is the chance he scores below 60, given he keeps a real role (1,000+ minutes); the arrow is his",
      "projected score in that case. The small line is the chance he either falls below 60 or loses the role.",
      sprintf("In the 2016-2025 backtest, %d%% of each year's top 10 who kept a real role fell below 60, against %d%% of",
              pct(bt$fall$top10_fell_if_role), pct(bt$fall$rest_fell_if_role)),
      sprintf("the rest of the top-quarter pool, and %d%% of the top 10 lost the role altogether.", pct(bt$fall$top10_lost_role)),
      "Odds above 50% have run high in past seasons, on few players. Age, a hot 3-point year, a one-season jump and a",
      "team change all raise the risk; a long record of strong seasons lowers it.", teams_note),
    rows = rows(lists$fall, "fall"))
)

tpl <- paste(readLines("board/template.html", encoding = "UTF-8"), collapse = "\n")
html <- sub("/*__DATA__*/null", toJSON(data, auto_unbox = TRUE, null = "null", digits = NA), tpl, fixed = TRUE)
dir.create("docs", showWarnings = FALSE)
writeLines(html, "docs/index.html", useBytes = TRUE)
