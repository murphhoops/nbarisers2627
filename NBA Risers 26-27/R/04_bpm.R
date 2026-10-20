# BPM for every player-season, with each part restated against the league
# average so the parts add up to his BPM.

library(data.table)
source("R/fn_bpm.R")

st <- readRDS("data/player_stints.rds")
ts <- readRDS("data/team_seasons.rds")

pb <- season_line(rbindlist(lapply(2002:2026, function(y) compute_bpm(st[season == y], ts[season == y]))))

# Raw parts carry a team constant of about -8, so compare each one to the
# league's minutes-weighted average. The creator/position baseline goes into
# scoring, since that's what it offsets.
parts <- c("b_scoring", "b_playmaking", "b_rebounding", "b_defense", "b_baseline", "b_team")
lg <- pb[mp >= 250, lapply(.SD, function(v) wmean(v, mp)), by = season, .SDcols = parts]
setnames(lg, parts, paste0("lg_", parts))
pb <- merge(pb, lg, by = "season")
pb[, `:=`(
  scoring      = (b_scoring + b_baseline) - (lg_b_scoring + lg_b_baseline),
  playmaking   = b_playmaking - lg_b_playmaking,
  rebounding   = b_rebounding - lg_b_rebounding,
  defense_box  = b_defense - lg_b_defense,
  team_context = b_team - lg_b_team
)]
pb[, paste0("lg_", parts) := NULL]

saveRDS(pb, "data/player_bpm.rds")
