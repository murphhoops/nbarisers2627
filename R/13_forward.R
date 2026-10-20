# Forward-in-time predictions for 2016-2025: each season predicted by models
# trained only on earlier seasons. Used for calibration and the backtests.

library(data.table)
source("R/fn_model.R")

d <- prep(readRDS("data/training_table.rds")[season <= 2025 & mp >= 200])
selected <- readRDS("data/selected_inputs.rds")
sel <- lapply(selected, `[[`, "selected")

vars <- unique(unlist(sel))
Xraw <- as.matrix(d[, lapply(.SD, as.numeric), .SDcols = vars])
X <- scale(Xraw)

fwd <- d[season >= 2016, .(season, athlete_id, player, value, age, exp_years, mp, mp_next,
                           value_next, real_role_next, breakout75)]
for (s in 2016:2025) {
  te <- d$season == s
  for (name in names(MODELS)) {
    m <- MODELS[[name]]
    tr <- d$season <= s - 1 & m$rows(d)
    b <- fit(design(X, sel[[name]], tr), m$y(d)[tr], m$type)
    p <- predict_b(design(X, sel[[name]], te), b, m$type)
    if (name == "proj") {
      # 80% range from the training seasons' errors
      q <- quantile(m$y(d)[tr] - predict_b(design(X, sel[[name]], tr), b, m$type), c(0.1, 0.9))
      fwd[season == s, `:=`(proj = p, proj_lo = pmax(0, p + q[1]), proj_hi = pmin(100, p + q[2]))]
    } else {
      fwd[season == s, (paste0("p_", name)) := p]
    }
  }
}
saveRDS(fwd, "data/forward_predictions.rds")

pr <- fwd[real_role_next == TRUE]
cat(sprintf("projected score if real role: RMSE %.1f, 80%% range covers %.0f%%, width %.0f\n",
            sqrt(mean((pr$value_next - pr$proj)^2)),
            100 * mean(pr$value_next >= pr$proj_lo & pr$value_next <= pr$proj_hi),
            mean(pr$proj_hi - pr$proj_lo)))
