# Match Basketball-Reference player-seasons to ESPN ids (for age), and check
# our BPM against theirs in every season.

library(data.table)

norm <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
  x <- gsub("-", " ", x)
  x <- tolower(gsub("[^A-Za-z ]", "", x))
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", x)
  gsub("\\s+", " ", trimws(x))
}

adv <- readRDS("data/bbref_advanced.rds")
# combined row for players who changed teams
adv[, multi := team == "TOT" | grepl("^[0-9]TM$", team)]
adv[, has_multi := any(multi), by = .(season, bbref_id)]
bb <- adv[(has_multi & multi) | !has_multi]
bb <- bb[, .SD[which.max(mp)], by = .(season, bbref_id)]
bb[, key := norm(player)]

pb <- readRDS("data/player_value.rds")
pb[, key := norm(player)]
pb[key == "michael sweetney", key := "mike sweetney"]

# same season and name, closest minutes. Allow 20% off when it's the only
# match: ESPN is missing 12 games from 2005-06.
cand <- merge(pb[, .(season, athlete_id, key, mp_espn = mp)],
              bb[, .(season, bbref_id, key, mp_bb = mp, age, bpm_bb = bpm, obpm_bb = obpm, dbpm_bb = dbpm)],
              by = c("season", "key"), allow.cartesian = TRUE)
cand[, gap := abs(mp_espn - mp_bb)]
cand[, n_cand := .N, by = .(season, athlete_id)]
cand <- cand[gap <= pmax(50, 0.10 * mp_bb) | (n_cand == 1 & gap <= 0.20 * mp_bb)]
setorder(cand, season, athlete_id, gap)
link <- cand[, .SD[1], by = .(season, athlete_id)]
setorder(link, season, bbref_id, gap)
link <- link[!duplicated(link[, .(season, bbref_id)])]

# second pass: first initial + last name, minutes within 2%
un_p <- pb[!link, on = .(season, athlete_id)]
un_b <- bb[!link, on = .(season, bbref_id)]
sig <- function(k) vapply(strsplit(k, " "), function(z) paste(substr(z[1], 1, 1), z[length(z)]), "")
un_p[, sig := sig(key)]
un_b[, sig := sig(key)]
c2 <- merge(un_p[, .(season, athlete_id, sig, mp_espn = mp)],
            un_b[, .(season, bbref_id, sig, mp_bb = mp, age, bpm_bb = bpm, obpm_bb = obpm, dbpm_bb = dbpm)],
            by = c("season", "sig"))
c2[, gap := abs(mp_espn - mp_bb)]
c2 <- c2[gap <= pmax(10, 0.02 * mp_bb)][, .SD[which.min(gap)], by = .(season, athlete_id)]
c2 <- c2[!duplicated(c2[, .(season, bbref_id)])]
link <- rbind(link, c2, fill = TRUE)

saveRDS(link[, .(season, athlete_id, bbref_id, age, bpm_bb, obpm_bb, dbpm_bb)], "data/bbref_link.rds")

# BPM check, 500+ minutes
v <- merge(pb[mp >= 500, .(season, athlete_id, bpm, obpm, dbpm)],
           link[, .(season, athlete_id, bpm_bb, obpm_bb, dbpm_bb)], by = c("season", "athlete_id"))
check <- v[, .(n = .N, r = cor(bpm, bpm_bb), mae = mean(abs(bpm - bpm_bb)),
               within_half = mean(abs(bpm - bpm_bb) <= 0.5)), by = season]
print(check[, lapply(.SD, function(x) round(x, 3))])
cat(sprintf("all seasons: n %d, r %.3f, MAE %.2f, within 0.5: %.0f%%\n", nrow(v),
            cor(v$bpm, v$bpm_bb), mean(abs(v$bpm - v$bpm_bb)), 100 * mean(abs(v$bpm - v$bpm_bb) <= 0.5)))
saveRDS(check, "data/bpm_check.rds")
