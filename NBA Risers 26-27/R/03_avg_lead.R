# Time-weighted average lead for each team-season, from play-by-play.
# BPM 2.0 gives teams that play with a lead a small bump (they coast).

library(data.table)

out <- list()
for (y in 2002:2026) {
  p <- as.data.table(readRDS(sprintf("data/raw/pbp/pbp_%d.rds", y)))[season_type == 2]
  p <- p[, .(game_id, period = as.integer(period_number), clock = clock_display_value,
             home = as.numeric(home_score), away = as.numeric(away_score),
             home_team_id, away_team_id, seq = as.numeric(sequence_number))]

  # clock is "MM:SS", or plain seconds in the last minute
  parts <- strsplit(p$clock, ":", fixed = TRUE)
  p[, clock_sec := vapply(parts, function(z) if (length(z) == 2) as.numeric(z[1]) * 60 + as.numeric(z[2]) else as.numeric(z[1]), 0)]
  p[, elapsed := ifelse(period <= 4, (period - 1) * 720 + (720 - clock_sec), 2880 + (period - 5) * 300 + (300 - clock_sec))]
  setorder(p, game_id, elapsed, seq)

  p[, margin := home - away]
  p[, next_t := shift(elapsed, type = "lead"), by = game_id]
  p[is.na(next_t), next_t := max(elapsed), by = game_id]
  p[, dur := pmax(0, next_t - elapsed)]

  g <- p[, .(home_team_id = home_team_id[1], away_team_id = away_team_id[1],
             avg_margin = sum(margin * dur) / sum(dur), len = max(elapsed)), by = game_id]
  g <- g[len >= 2800]   # skip truncated games
  tl <- rbind(g[, .(team_id = home_team_id, lead = avg_margin)],
              g[, .(team_id = away_team_id, lead = -avg_margin)])
  out[[as.character(y)]] <- tl[, .(season = y, games_pbp = .N, avg_lead = mean(lead)), by = team_id]
}

saveRDS(rbindlist(out), "data/team_avg_lead.rds")
