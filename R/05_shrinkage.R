# How hard to shrink small samples. Shrunk BPM blends a player's BPM with what
# players at his minutes usually post, the prior counting as K minutes:
#   (BPM * MP + prior * K) / (MP + K),  prior = -4.75 + 0.175 * MP / (G + 4)
# Pick the K whose shrunk BPM best predicts next season's BPM (target: next
# season 500+ min), scored leaving one season out at a time.

library(data.table)

pb <- readRDS("data/player_bpm.rds")
nxt <- pb[, .(athlete_id, season = season - 1, bpm_next = bpm, mp_next = mp)]
d <- merge(pb[season <= 2025], nxt, by = c("athlete_id", "season"))[mp_next >= 500]
d[, prior := -4.75 + 0.175 * mp / (g + 4)]

loso_rmse <- function(x) {
  err <- numeric(0)
  for (s in unique(d$season)) {
    tr <- d$season != s
    b <- lm.fit(cbind(1, x[tr]), d$bpm_next[tr])$coefficients
    err <- c(err, d$bpm_next[!tr] - (b[1] + b[2] * x[!tr]))
  }
  sqrt(mean(err^2))
}

Ks <- c(0, 50, 100, 150, 200, 300, 400, 500, 600, 800, 1000, 1250, 1500, 2000, 3000)
grid <- data.table(K = Ks, rmse = sapply(Ks, function(K) loso_rmse((d$bpm * d$mp + d$prior * K) / (d$mp + K))))
print(grid)

saveRDS(list(best_K = grid[which.min(rmse), K], grid = grid), "data/shrinkage.rds")
