# One row per player-season with every candidate input and next-season targets.
# 2002-2025 rows are for training; 2026 rows get scored for 2026-27.

library(data.table)

pb   <- readRDS("data/player_value.rds")
st   <- readRDS("data/player_stints.rds")
ts   <- readRDS("data/team_seasons.rds")
link <- readRDS("data/bbref_link.rds")
bio  <- readRDS("data/bbref_bio.rds")
dr   <- readRDS("data/bbref_draft.rds")
gf   <- readRDS("data/shots_trend.rds")
ro   <- as.data.table(readRDS("data/raw/rosters_2027.rds"))

slen <- ts[, .(season_games = max(g)), by = season]
slen[, len_factor := season_games / 82]

# box score rates, using team stats while he was on the floor
s <- merge(st, ts[, .(season, team_id, t_g = g, t_mp = team_mp, t_poss = poss, t_pts = pts,
                      t_opp = opp_pts, t_fgm = fgm, t_fga = fga, t_fta = fta, t_tov = tov,
                      t_orb = orb, t_drb = drb, t_o_orb = o_orb, t_o_drb = o_drb, adj_net)],
           by = c("season", "team_id"))
s[, on_share := mp / (t_mp / 5)]
box <- s[, .(
  g = sum(g), gs = sum(gs), mp = sum(mp), n_teams = .N, last_team = team_id[.N],
  fgm = sum(fgm), fga = sum(fga), tpm = sum(tpm), tpa = sum(tpa), ftm = sum(ftm), fta = sum(fta),
  orb = sum(orb), drb = sum(drb), ast = sum(ast), stl = sum(stl), blk = sum(blk), tov = sum(tov),
  pf = sum(pf), pts = sum(pts), pm = sum(pm),
  on_poss = sum(t_poss * on_share), tm_fgm_on = sum(t_fgm * on_share),
  tm_orb_on = sum(t_orb * on_share), tm_drb_on = sum(t_drb * on_share),
  opp_orb_on = sum(t_o_orb * on_share), opp_drb_on = sum(t_o_drb * on_share),
  tm_use_on = sum((t_fga + 0.44 * t_fta + t_tov) * on_share),
  team_margin = sum(t_pts - t_opp), team_poss = sum(t_poss),
  team_adj_net = sum(adj_net * mp) / sum(mp)
), by = .(season, athlete_id)]
box <- merge(box, slen, by = "season")
box[, `:=`(
  min_share = mp / (season_games * 48),
  mpg = mp / g,
  starts_share = gs / g,
  games_share = pmin(1, g / season_games),
  usg = 100 * (fga + 0.44 * fta + tov) / tm_use_on,
  ts_pct = pts / (2 * (fga + 0.44 * fta)),
  three_rate_box = fifelse(fga > 0, tpa / fga, NA_real_),
  ft_rate = fifelse(fga > 0, fta / fga, NA_real_),
  ast_pct = 100 * ast / pmax(1, tm_fgm_on - fgm),
  tov_pct = 100 * tov / pmax(1, fga + 0.44 * fta + tov),
  orb_pct = 100 * orb / (tm_orb_on + opp_drb_on),
  drb_pct = 100 * drb / (tm_drb_on + opp_orb_on),
  stl100 = 100 * stl / on_poss, blk100 = 100 * blk / on_poss, pf100 = 100 * pf / on_poss,
  pm100 = 100 * pm / on_poss,
  on_off = 100 * pm / on_poss - 100 * (team_margin - pm) / pmax(1, team_poss - on_poss)
)]
lgsh <- box[, .(lg_tp = sum(tpm) / sum(tpa), lg_ft = sum(ftm) / sum(fta),
                lg_ts = sum(pts) / (2 * sum(fga + 0.44 * fta))), by = season]
box <- merge(box, lgsh, by = "season")
box[, tp_pct_shr := (tpm + 100 * lg_tp) / (tpa + 100)]   # 100-attempt prior
box[, ft_pct_shr := (ftm + 50 * lg_ft) / (fta + 50)]     # 50-attempt prior
box[, rts := ts_pct - lg_ts]
box[, three_rate_box_rel := three_rate_box - mean(three_rate_box, na.rm = TRUE), by = season]

d <- merge(pb[, .(season, athlete_id, player, teams, value, bpm, obpm, dbpm, scoring, playmaking,
                  rebounding, defense_box, team_context, position, role, prior)],
           box, by = c("season", "athlete_id"))

# history
setorder(d, athlete_id, season)
p1 <- d[, .(athlete_id, season = season + 1, value_p1 = value, bpm_p1 = bpm, mp_p1 = mp)]
p2 <- d[, .(athlete_id, season = season + 2, value_p2 = value, bpm_p2 = bpm, mp_p2 = mp)]
d <- merge(d, p1, by = c("athlete_id", "season"), all.x = TRUE)
d <- merge(d, p2, by = c("athlete_id", "season"), all.x = TRUE)
d[, value_chg1 := value - value_p1]
d[, value_chg2 := value - value_p2]
d[, mp_prev := fcoalesce(mp_p1, 0)]
# multi-year BPM: last season's minutes count 0.25, two back 0.0625, plus the prior
d[, multi_est := (bpm * mp + 0.25 * fcoalesce(mp_p1, 0) * fcoalesce(bpm_p1, 0) +
                  0.0625 * fcoalesce(mp_p2, 0) * fcoalesce(bpm_p2, 0) + prior * 300) /
                 (mp + 0.25 * fcoalesce(mp_p1, 0) + 0.0625 * fcoalesce(mp_p2, 0) + 300)]
d[, seasons_on_file := seq_len(.N) - 1L, by = athlete_id]
d[, career_peak_value := cummax(value), by = athlete_id]

# age, experience, bio, draft
d <- merge(d, link[, .(season, athlete_id, bbref_id, age)], by = c("season", "athlete_id"), all.x = TRUE)
d <- merge(d, bio[, .(bbref_id, year_min, height_in, weight, college)], by = "bbref_id", all.x = TRUE)
d <- merge(d, dr[, .SD[which.min(draft_year)], by = bbref_id][, .(bbref_id, pick)], by = "bbref_id", all.x = TRUE)
d[, exp_years := season - year_min]
d[, undrafted := !is.na(bbref_id) & is.na(pick)]
d[, pick_filled := fifelse(undrafted, 61L, pick)]

# shot profile, relative to the league each season
d <- merge(d, gf, by = c("season", "athlete_id"), all.x = TRUE)
for (v in c("rim_rate", "short_mid_rate", "long_mid_rate", "corner3_share", "rim_fg", "mid_fg", "assisted_share")) {
  d[, paste0(v, "_rel") := get(v) - mean(get(v)[fga_pbp >= 100], na.rm = TRUE), by = season]
}
d[fga_pbp < 50, c("rim_rate_rel", "short_mid_rate_rel", "long_mid_rate_rel", "assisted_share_rel") := NA]
d[rim_att < 25, rim_fg_rel := NA]
d[mid_att < 25, mid_fg_rel := NA]
d[is.na(three_rate) | three_rate * fga_pbp < 25, corner3_share_rel := NA]

d[, traded_midseason := n_teams > 1]

# next season
nx <- d[, .(athlete_id, season = season - 1, value_next = value, mp_next = mp, team_next = last_team)]
d <- merge(d, nx, by = c("athlete_id", "season"), all.x = TRUE)
d <- merge(d, slen[, .(season = season - 1, len_next = len_factor)], by = "season", all.x = TRUE)
d[season <= 2025 & is.na(mp_next), mp_next := 0]
d[, changed_team_next := fifelse(season <= 2025 & mp_next > 0, team_next != last_team, NA)]

# 2026-27 team from current rosters; no team listed counts as a change
r27 <- ro[, .(athlete_id = as.integer(athlete_id), team27 = as.integer(team_id))]
d <- merge(d, r27, by = "athlete_id", all.x = TRUE)
d[season == 2026, changed_team_next := is.na(team27) | team27 != last_team]

# a real role = 1,000 minutes in an 82-game season
d[, real_role_next := mp_next >= 1000 * fcoalesce(len_next, 1)]
d[, breakout75 := real_role_next & value_next >= 75]

saveRDS(d, "data/training_table.rds")
