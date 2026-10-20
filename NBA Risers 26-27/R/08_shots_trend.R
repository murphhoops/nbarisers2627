# Shot profile from play-by-play and within-season trend from box scores.
# Shot coordinates are full court from center; the hoops sit at x = +/-41.75 ft.

library(data.table)

shots <- list()
trend <- list()

for (y in 2002:2026) {
  p <- as.data.table(readRDS(sprintf("data/raw/pbp/pbp_%d.rds", y)))[
    season_type == 2 & shooting_play == TRUE & !grepl("Free Throw", type_text)]
  p <- p[, .(athlete_id = as.integer(athlete_id_1), text, made = scoring_play %in% TRUE,
             x = as.numeric(coordinate_x), yy = as.numeric(coordinate_y))][!is.na(athlete_id)]
  p[, three := grepl("three point", tolower(text))]
  p[, dist := sqrt((abs(x) - 41.75)^2 + yy^2)]
  p[, assisted := made & grepl("assist", tolower(text))]
  p[, zone := fifelse(three, "three", fifelse(dist < 4, "rim", fifelse(dist < 14, "short_mid", "long_mid")))]
  p[, corner := three & abs(x) >= 33]

  shots[[as.character(y)]] <- p[, .(
    season = y, fga_pbp = .N,
    rim_rate = mean(zone == "rim"), short_mid_rate = mean(zone == "short_mid"),
    long_mid_rate = mean(zone == "long_mid"), three_rate = mean(three),
    corner3_share = if (any(three)) mean(corner[three]) else NA_real_,
    rim_att = sum(zone == "rim"),
    rim_fg = if (any(zone == "rim")) mean(made[zone == "rim"]) else NA_real_,
    mid_att = sum(zone %in% c("short_mid", "long_mid")),
    mid_fg = if (any(zone %in% c("short_mid", "long_mid"))) mean(made[zone %in% c("short_mid", "long_mid")]) else NA_real_,
    assisted_share = if (any(made)) mean(assisted[made]) else NA_real_
  ), by = athlete_id]

  # Game Score per 36, second half of his games minus the first half
  num <- function(z) suppressWarnings(as.numeric(z))
  b <- as.data.table(readRDS(sprintf("data/raw/player_box_%d.rds", y)))[season_type == 2]
  b <- b[, .(athlete_id = as.integer(athlete_id), game_date = as.Date(game_date), mp = num(minutes),
             pts = num(points), fgm = num(field_goals_made), fga = num(field_goals_attempted),
             ftm = num(free_throws_made), fta = num(free_throws_attempted),
             orb = num(offensive_rebounds), drb = num(defensive_rebounds), stl = num(steals),
             ast = num(assists), blk = num(blocks), pf = num(fouls), tov = num(turnovers))][!is.na(mp) & mp > 0]
  b[, gmsc := pts + 0.4 * fgm - 0.7 * fga - 0.4 * (fta - ftm) + 0.7 * orb + 0.3 * drb +
               stl + 0.7 * ast + 0.7 * blk - 0.4 * pf - tov]
  setorder(b, athlete_id, game_date)
  b[, half := ifelse(seq_len(.N) <= .N / 2, 1L, 2L), by = athlete_id]
  h <- dcast(b[, .(mp = sum(mp), gmsc36 = 36 * sum(gmsc) / sum(mp)), by = .(athlete_id, half)],
             athlete_id ~ half, value.var = c("mp", "gmsc36"))
  trend[[as.character(y)]] <- h[mp_1 >= 150 & mp_2 >= 150, .(season = y, athlete_id, gmsc36_trend = gmsc36_2 - gmsc36_1)]
}

saveRDS(merge(rbindlist(shots), rbindlist(trend), by = c("season", "athlete_id"), all = TRUE),
        "data/shots_trend.rds")
