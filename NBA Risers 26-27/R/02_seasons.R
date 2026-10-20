# Player stints (player x team x season) and team seasons from ESPN box scores.
# Regular season only, 2001-02 through 2025-26.

library(data.table)

num <- function(x) suppressWarnings(as.numeric(x))

load_season <- function(y) {
  p <- as.data.table(readRDS(sprintf("data/raw/player_box_%d.rds", y)))[season_type == 2]
  t <- as.data.table(readRDS(sprintf("data/raw/team_box_%d.rds", y)))[season_type == 2]

  # drop All-Star and exhibition teams
  real <- t[, .N, by = team_id][N >= 50, team_id]
  t <- t[team_id %in% real & opponent_team_id %in% real]

  # the NBA Cup final doesn't count toward regular-season stats
  gp <- t[, .N, by = team_id]
  normal <- as.integer(names(sort(table(gp$N), decreasing = TRUE))[1])
  extra <- gp[N == normal + 1, team_id]
  if (length(extra) == 2) {
    cup <- t[team_id == extra[1] & opponent_team_id == extra[2]]
    cup <- cup[format(as.Date(game_date), "%m") == "12"]
    if (nrow(cup) >= 1) t <- t[!game_id %in% cup$game_id[nrow(cup)]]
  }
  list(p = p[game_id %in% t$game_id & team_id %in% real], t = t)
}

stints <- list()
teams <- list()

for (y in 2002:2026) {
  s <- load_season(y)
  p <- s$p
  t <- s$t
  p[, mp := num(minutes)]
  p <- p[!is.na(mp) & mp > 0]

  stints[[as.character(y)]] <- p[, .(
    season = y,
    player = athlete_display_name[.N], pos_listed = athlete_position_abbreviation[.N],
    team_abbr = team_abbreviation[.N],
    g = .N, gs = sum(starter %in% TRUE), mp = sum(mp),
    fgm = sum(num(field_goals_made)), fga = sum(num(field_goals_attempted)),
    tpm = sum(num(three_point_field_goals_made)), tpa = sum(num(three_point_field_goals_attempted)),
    ftm = sum(num(free_throws_made)), fta = sum(num(free_throws_attempted)),
    orb = sum(num(offensive_rebounds)), drb = sum(num(defensive_rebounds)),
    ast = sum(num(assists)), stl = sum(num(steals)), blk = sum(num(blocks)),
    tov = sum(num(turnovers)), pf = sum(num(fouls)), pts = sum(num(points)),
    pm = sum(num(plus_minus), na.rm = TRUE)
  ), by = .(athlete_id, team_id)]

  # team counting stats from player rows (team-box rebounds are wrong in 2007-08)
  pg <- p[, .(fgm = sum(num(field_goals_made)), fga = sum(num(field_goals_attempted)),
              tpa = sum(num(three_point_field_goals_attempted)), fta = sum(num(free_throws_attempted)),
              orb = sum(num(offensive_rebounds)), drb = sum(num(defensive_rebounds)),
              ast = sum(num(assists)), stl = sum(num(steals)), blk = sum(num(blocks)),
              tov_pl = sum(num(turnovers)), pf = sum(num(fouls))), by = .(game_id, team_id)]
  tg <- t[, .(game_id, team_id, opp = opponent_team_id, home = team_home_away == "home",
              pts = num(team_score), opp_pts = num(opponent_team_score),
              tov_col = num(turnovers), tov_tot = num(total_turnovers))]
  tg <- merge(tg, pg, by = c("game_id", "team_id"))

  # team turnovers incl. team TOs; ESPN's columns mean different things by era.
  # Through 2012 `turnovers` already includes them and `total_turnovers` double counts.
  # 2017-18 has no team TOs, so add 0.56/game (2017 and 2019 averaged 0.58 and 0.55).
  if (y <= 2012) tg[, tov := pmax(tov_col, tov_pl)] else tg[, tov := tov_tot]
  tg[is.na(tov), tov := tov_pl + 0.56]

  o <- tg[, .(game_id, team_id = opp, o_fgm = fgm, o_fga = fga, o_fta = fta,
              o_orb = orb, o_drb = drb, o_tov = tov)]
  tg <- merge(tg, o, by = c("game_id", "team_id"))
  poss1 <- with(tg, fga + 0.4 * fta - 1.07 * (orb / (orb + o_drb)) * (fga - fgm) + tov)
  poss2 <- with(tg, o_fga + 0.4 * o_fta - 1.07 * (o_orb / (o_orb + drb)) * (o_fga - o_fgm) + o_tov)
  tg[, poss := 0.5 * (poss1 + poss2)]
  tg <- merge(tg, p[, .(team_mp = sum(mp)), by = .(game_id, team_id)], by = c("game_id", "team_id"))

  # schedule-adjusted net and offensive ratings (least squares, home court +/-)
  tg[, margin100 := 100 * (pts - opp_pts) / poss]
  tg[, off100 := 100 * pts / poss]
  ids <- sort(unique(tg$team_id))
  n <- nrow(tg)
  hm <- ifelse(tg$home, 0.5, -0.5)
  X <- matrix(0, n, length(ids))
  X[cbind(seq_len(n), match(tg$team_id, ids))] <- 1
  X[cbind(seq_len(n), match(tg$opp, ids))] <- -1
  net <- c(0, lm.fit(cbind(hm, X[, -1]), tg$margin100)$coefficients[-1])
  net <- net - mean(net)

  Xo <- matrix(0, n, 2 * length(ids))
  Xo[cbind(seq_len(n), match(tg$team_id, ids))] <- 1
  Xo[cbind(seq_len(n), length(ids) + match(tg$opp, ids))] <- 1
  lg_off <- mean(tg$off100)
  fo <- c(lm.fit(cbind(hm, Xo[, -ncol(Xo)]), tg$off100 - lg_off)$coefficients, 0)
  off <- fo[2:(length(ids) + 1)]
  off <- off - mean(off)

  ts <- tg[, .(g = .N, team_mp = sum(team_mp), poss = sum(poss), pts = sum(pts), opp_pts = sum(opp_pts),
               fga = sum(fga), fta = sum(fta), fgm = sum(fgm), tpa = sum(tpa), orb = sum(orb),
               drb = sum(drb), o_orb = sum(o_orb), o_drb = sum(o_drb), ast = sum(ast),
               stl = sum(stl), blk = sum(blk), tov = sum(tov), pf = sum(pf)), by = team_id]
  ts[, `:=`(season = y, net_raw = 100 * (pts - opp_pts) / poss,
            adj_net = net[match(team_id, ids)], adj_off = off[match(team_id, ids)], lg_off100 = lg_off)]
  teams[[as.character(y)]] <- ts
}

saveRDS(rbindlist(stints), "data/player_stints.rds")
saveRDS(rbindlist(teams), "data/team_seasons.rds")
