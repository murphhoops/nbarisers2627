# 0-100 value score: within-season percentile of shrunk BPM.
# Ranked against players with 500+ minutes that season, so 75 = top quarter.
# Players under 500 minutes are placed on the same scale.

library(data.table)

K <- readRDS("data/shrinkage.rds")$best_K
pb <- readRDS("data/player_bpm.rds")

pb[, prior := -4.75 + 0.175 * mp / (g + 4)]
pb[, bpm_reg := (bpm * mp + prior * K) / (mp + K)]
pb[, in_pool := mp >= 500]
pb[, value := {
  pool <- sort(bpm_reg[in_pool])
  below <- findInterval(bpm_reg, pool, left.open = TRUE)
  at_or_below <- findInterval(bpm_reg, pool)
  100 * (below + 0.5 * (at_or_below - below)) / length(pool)
}, by = season]
pb[, value := pmin(100, pmax(0, value))]

saveRDS(pb, "data/player_value.rds")
