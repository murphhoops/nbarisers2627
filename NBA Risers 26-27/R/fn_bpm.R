# BPM 2.0 (Daniel Myers), rebuilt from box scores.
# Coefficients from basketball-reference.com/about/bpm2.html.
#
# Two things the page doesn't print:
#   - the points baseline. 1.00 pts per true shot attempt reproduces their 2017
#     LeBron example exactly (raw 15.6, team adjustment -8.0, BPM 7.6).
#   - how the lead adjustment splits between offense and defense. Half to
#     offense matched their OBPM/DBPM best.

library(data.table)

# position-varying terms: value at position 1 (PG) and 5 (C), linear in between
COEF <- list(
  bpm = list(
    pts = c(0.860, 0.860), tpm = c(0.389, 0.389), ast = c(0.580, 1.034),
    tov = c(-0.964, -0.964), orb = c(0.613, 0.181), drb = c(0.116, 0.181),
    stl = c(1.369, 1.008), blk = c(1.327, 0.703), pf = c(-0.367, -0.367),
    fga_role = c(-0.560, -0.780),   # role 1 (creator) to role 5 (receiver)
    pos_const1 = -0.818,
    role_const1 = -2.774),
  obpm = list(
    pts = c(0.605, 0.605), tpm = c(0.477, 0.477), ast = c(0.476, 0.476),
    tov = c(-0.579, -0.882), orb = c(0.606, 0.422), drb = c(-0.112, 0.103),
    stl = c(0.177, 0.294), blk = c(0.725, 0.097), pf = c(-0.439, -0.439),
    fga_role = c(-0.330, -0.472),
    pos_const1 = -1.698,
    role_const1 = -0.860)
)
FTA_PER_FGA <- 0.44
PPTSA_BASELINE <- 1.00
LEAD_EFFECT <- 0.35     # pts/100 given up per point of average lead
LEAD_PER_MOV <- 0.57    # avg lead per point of margin, for seasons with thin pbp

POS_REG  <- c(int = 2.130, trb = 8.668, stl = -2.486, pf = 0.992, ast = -3.536, blk = 1.667)
ROLE_REG <- c(int = 6.00, ast = -6.642, thr = -8.544)
LISTED_POS <- c(PG = 1, SG = 2, G = 1.5, SF = 3, F = 3.5, GF = 2.5, PF = 4, FC = 4.5, C = 5)

wmean <- function(x, w) sum(x * w) / sum(w)

# shift so the team's minutes-weighted average is 3, clamp to 1-5, repeat
center_team <- function(x, w) {
  for (i in 1:50) {
    x <- pmin(5, pmax(1, x + (3 - wmean(x, w))))
    if (abs(wmean(x, w) - 3) < 1e-6) break
  }
  x
}

add_lead <- function(ts) {
  lead <- readRDS("data/team_avg_lead.rds")
  ts <- merge(ts, lead[, .(season, team_id, games_pbp, avg_lead)], by = c("season", "team_id"), all.x = TRUE)
  ts[, mov := (pts - opp_pts) / g]
  ts[is.na(games_pbp) | games_pbp / g < 0.9, avg_lead := LEAD_PER_MOV * mov]
  ts[]
}

compute_bpm <- function(st, ts) {
  ts <- add_lead(copy(ts))
  ts[, rating := adj_net + LEAD_EFFECT / 2 * avg_lead]
  ts[, rating_off := adj_off + 0.5 * LEAD_EFFECT / 2 * avg_lead]
  d <- merge(st, ts[, .(season, team_id, t_mp = team_mp, t_poss = poss, t_pts = pts,
                        t_fga = fga, t_fta = fta, t_trb = orb + drb, t_stl = stl,
                        t_pf = pf, t_ast = ast, t_blk = blk, rating, rating_off)],
             by = c("season", "team_id"))
  d[, on_share := mp / (t_mp / 5)]
  d[, on_poss := t_poss * on_share]
  d[, tsa := fga + FTA_PER_FGA * fta]
  d[, t_tsa := t_fga + FTA_PER_FGA * t_fta]
  d[, t_pptsa := t_pts / t_tsa]

  # position and offensive role from share of team stats while on court
  pct <- function(x, tx) x / (tx * d$on_share)
  d[, pos_raw := POS_REG["int"] + POS_REG["trb"] * pct(orb + drb, t_trb) +
                 POS_REG["stl"] * pct(stl, t_stl) + POS_REG["pf"] * pct(pf, t_pf) +
                 POS_REG["ast"] * pct(ast, t_ast) + POS_REG["blk"] * pct(blk, t_blk)]
  d[, listed := LISTED_POS[gsub("-", "", pos_listed)]]
  d[is.na(listed), listed := 3]
  d[, pos_blend := pmin(5, pmax(1, (pos_raw * mp + listed * 50) / (mp + 50)))]
  d[, position := center_team(pos_blend, mp), by = .(season, team_id)]

  d[, thr_line := t_pptsa - 0.33]
  d[, thr_pts := pts - tsa * thr_line]
  d[, t_thr := t_pts - t_tsa * thr_line]
  d[, role_raw := ROLE_REG["int"] + ROLE_REG["ast"] * pct(ast, t_ast) +
                  ROLE_REG["thr"] * (thr_pts / (t_thr * on_share))]
  d[, role_blend := pmin(5, pmax(1, (role_raw * mp + 4 * 50) / (mp + 50)))]
  d[, role := center_team(role_blend, mp), by = .(season, team_id)]

  p100 <- function(x) 100 * x / d$on_poss
  d[, pts_adj := pts + tsa * (PPTSA_BASELINE - t_pptsa)]

  terms <- function(cf, prefix) {
    w <- (d$position - 1) / 4
    r <- (d$role - 1) / 4
    ip <- function(v) v[1] + w * (v[2] - v[1])
    fga_c <- cf$fga_role[1] + r * (cf$fga_role[2] - cf$fga_role[1])
    out <- data.table(
      scoring    = ip(cf$pts) * p100(d$pts_adj) + ip(cf$tpm) * p100(d$tpm) +
                   fga_c * p100(d$fga) + FTA_PER_FGA * fga_c * p100(d$fta),
      playmaking = ip(cf$ast) * p100(d$ast) + ip(cf$tov) * p100(d$tov),
      rebounding = ip(cf$orb) * p100(d$orb) + ip(cf$drb) * p100(d$drb),
      defense    = ip(cf$stl) * p100(d$stl) + ip(cf$blk) * p100(d$blk) + ip(cf$pf) * p100(d$pf),
      baseline   = ifelse(d$position < 3, cf$pos_const1 * (3 - d$position) / 2, 0) +
                   cf$role_const1 * (3 - d$role) / 2
    )
    out[, raw := scoring + playmaking + rebounding + defense + baseline]
    setnames(out, paste0(prefix, names(out)))
    out
  }
  d <- cbind(d, terms(COEF$bpm, "b_"), terms(COEF$obpm, "o_"))

  # team adjustment: minutes-weighted team total has to equal the team rating
  d[, b_team := (rating - sum(on_share * b_raw)) / 5, by = .(season, team_id)]
  d[, o_team := (rating_off - sum(on_share * o_raw)) / 5, by = .(season, team_id)]
  d[, bpm := b_raw + b_team]
  d[, obpm := o_raw + o_team]
  d[, dbpm := bpm - obpm]
  d[]
}

# one line per player-season, minutes-weighted across teams
season_line <- function(d) {
  vals <- c("bpm", "obpm", "dbpm", "position", "role", "b_scoring", "b_playmaking",
            "b_rebounding", "b_defense", "b_baseline", "b_team")
  d[, c(list(player = player[which.max(mp)], teams = paste(team_abbr, collapse = "/"),
             g = sum(g), mp = sum(mp)),
        lapply(.SD, function(v) wmean(v, mp))),
    by = .(season, athlete_id), .SDcols = vals]
}
