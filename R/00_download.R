# Download ESPN box scores, play-by-play and current rosters from the
# sportsdataverse data releases. Files already on disk are skipped.

options(timeout = 600)
base <- "https://github.com/sportsdataverse/sportsdataverse-data/releases/download"
seasons <- 2002:2026

dir.create("data/raw/pbp", recursive = TRUE, showWarnings = FALSE)

get <- function(url, dest) {
  if (!file.exists(dest)) download.file(url, dest, mode = "wb", quiet = TRUE)
}

for (y in seasons) {
  get(sprintf("%s/espn_nba_player_boxscores/player_box_%d.rds", base, y), sprintf("data/raw/player_box_%d.rds", y))
  get(sprintf("%s/espn_nba_team_boxscores/team_box_%d.rds", base, y), sprintf("data/raw/team_box_%d.rds", y))
  get(sprintf("%s/espn_nba_pbp/play_by_play_%d.rds", base, y), sprintf("data/raw/pbp/pbp_%d.rds", y))
}

# 2026-27 rosters, used for offseason team changes
get(sprintf("%s/espn_nba_rosters/rosters_2027.rds", base), "data/raw/rosters_2027.rds")
