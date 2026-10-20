# Platt scaling on the forward predictions: logit(p_new) = a + b * logit(p).
# Tested honestly (each season corrected with a fit on earlier seasons only)
# and kept only for models where it lowers log loss on 2018-2025.

library(data.table)
source("R/fn_model.R")

f <- readRDS("data/forward_predictions.rds")
f[, hit60 := as.numeric(real_role_next %in% TRUE & value_next >= 60)]
f[, fell := as.numeric(value_next < 60)]

logloss <- function(p, y) -mean(y * log(p) + (1 - y) * log(1 - p))

calibrate <- function(rows, pcol, ycol) {
  x <- rows[, .(season, p = pmin(pmax(get(pcol), 1e-4), 1 - 1e-4), y = as.numeric(get(ycol)))]
  x[, p_cal := NA_real_]
  for (s in 2018:2025) {
    g <- glm(y ~ logit(p), data = x[season <= s - 1], family = binomial())
    x[season == s, p_cal := predict(g, newdata = .SD, type = "response")]
  }
  t <- x[season >= 2018]
  g <- glm(y ~ logit(p), data = x, family = binomial())
  list(coef = coef(g), use = logloss(t$p_cal, t$y) < logloss(t$p, t$y),
       logloss_raw = logloss(t$p, t$y), logloss_cal = logloss(t$p_cal, t$y))
}

cal <- list(
  role     = calibrate(f, "p_role", "real_role_next"),
  breakout = calibrate(f[value <= 75 & real_role_next == TRUE], "p_breakout", "breakout75"),
  d60      = calibrate(f[value < 60 & real_role_next == TRUE], "p_d60", "hit60"),
  fall     = calibrate(f[value >= 75 & real_role_next == TRUE], "p_fall", "fell")
)
print(rbindlist(lapply(names(cal), function(n) data.table(model = n, raw = round(cal[[n]]$logloss_raw, 4),
                                                          corrected = round(cal[[n]]$logloss_cal, 4), use = cal[[n]]$use))))
saveRDS(cal, "data/calibration.rds")
