# Fit every model on all seasons and score 2025-26 players for 2026-27.

library(data.table)
source("R/fn_model.R")

d <- prep(readRDS("data/training_table.rds")[mp >= 200])
selected <- readRDS("data/selected_inputs.rds")
cal <- readRDS("data/calibration.rds")
sel <- lapply(selected, `[[`, "selected")

vars <- unique(unlist(sel))
Xraw <- as.matrix(d[, lapply(.SD, as.numeric), .SDcols = vars])
train <- d$season <= 2025
X <- scale(Xraw, center = colMeans(Xraw[train, ]), scale = apply(Xraw[train, ], 2, sd))
te <- d$season == 2026

pred <- d[te, .(athlete_id, player, last_team, team27, age, exp_years, g, mp, value, bpm,
                scoring, playmaking, rebounding, defense_box, team_context, multi_est, value_chg1)]
for (name in names(MODELS)) {
  m <- MODELS[[name]]
  tr <- train & m$rows(d)
  b <- fit(design(X, sel[[name]], tr), m$y(d)[tr], m$type)
  p <- predict_b(design(X, sel[[name]], te), b, m$type)
  if (name == "proj") {
    q <- quantile(m$y(d)[tr] - predict_b(design(X, sel[[name]], tr), b, m$type), c(0.1, 0.9))
    pred[, `:=`(proj = p, proj_lo = pmax(0, p + q[1]), proj_hi = pmin(100, p + q[2]))]
  } else {
    pred[, (paste0("p_", name)) := apply_cal(p, cal[[name]])]
  }
}
pred[, p_top_quarter := p_breakout * p_role]
pred[, p_60 := p_d60 * p_role]
pred[, p_off := 1 - p_role * (1 - p_fall)]

abbr <- unique(rbind(readRDS("data/player_stints.rds")[season == 2026, .(team_id, team_abbr)],
                     as.data.table(readRDS("data/raw/rosters_2027.rds"))[, .(team_id = as.integer(team_id), team_abbr = team_abbreviation)]))
abbr <- abbr[, .SD[1], by = team_id]
pred[abbr, on = .(last_team = team_id), team_last := i.team_abbr]
pred[abbr, on = .(team27 = team_id), team_next := i.team_abbr]
pred[, status := fifelse(is.na(team27), "no team listed", fifelse(team27 != last_team, "moved", "stay"))]

lists <- list(
  breakout = pred[exp_years <= 4 & mp >= 300 & value <= 75][order(-p_breakout)][1:25],
  diamond  = pred[value < 50 & !is.na(age) & age <= 27][order(-p_60)][1:25],
  fall     = pred[value >= 75 & mp >= 1000][order(-p_fall)][1:25]
)
saveRDS(pred, "data/predictions_2026_27.rds")
saveRDS(lists, "data/board_lists.rds")

# readable copy of the predictions for the repo
dir.create("output", showWarnings = FALSE)
out <- pred[, .(player, team_2025_26 = team_last, team_2026_27 = fifelse(is.na(team_next), "", team_next),
                age, minutes = round(mp), score = round(value), bpm = round(bpm, 1),
                projected_if_real_role = round(proj), range_low = round(proj_lo), range_high = round(proj_hi),
                chance_real_role = round(p_role, 3), breakout_pct = round(p_breakout, 3),
                breakout_pct_overall = round(p_top_quarter, 3), chance_60_plus = round(p_60, 3),
                chance_fall_below_60 = round(p_fall, 3))][order(-score)]
# each chance only means something for the players it was built for
out[score > 75, c("breakout_pct", "breakout_pct_overall") := NA]
out[score >= 60, chance_60_plus := NA]
out[score < 75, chance_fall_below_60 := NA]
fwrite(out, "output/predictions_2026_27.csv")
