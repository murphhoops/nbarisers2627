# Shared pieces for the models: candidate inputs, fills, design matrix.

library(data.table)

# 52 inputs left after 10_screen.R. Dropped for overlap: mp and trust (kept
# min_share), bpm (kept value), years since draft (kept exp_years and age),
# the rebounding BPM part (kept orb/drb %), raw TS% (kept rts), pbp 3-point
# rate (kept the box score one), BPM change (kept value change).
CANDIDATES <- c(
  "value", "multi_est", "obpm", "dbpm", "scoring", "playmaking", "defense_box", "team_context",
  "value_chg1", "value_chg2", "seasons_on_file", "exp_years", "career_peak_value", "mp_prev", "gmsc36_trend",
  "age",
  "mpg", "min_share", "starts_share", "games_share", "usg", "position", "role",
  "rts", "three_rate_box_rel", "tp_pct_shr", "ft_pct_shr", "ft_rate", "ast_pct", "tov_pct",
  "orb_pct", "drb_pct", "stl100", "blk100", "pf100",
  "rim_rate_rel", "short_mid_rate_rel", "long_mid_rate_rel", "corner3_share_rel", "rim_fg_rel",
  "mid_fg_rel", "assisted_share_rel",
  "pm100", "on_off",
  "pick_filled", "undrafted", "height_in", "weight", "college",
  "team_adj_net", "traded_midseason", "changed_team_next")

# model name -> which rows it trains on and what it predicts
MODELS <- list(
  role     = list(type = "logit",  rows = function(d) rep(TRUE, nrow(d)),
                  y = function(d) as.numeric(d$real_role_next)),
  breakout = list(type = "logit",  rows = function(d) d$value <= 75 & d$real_role_next %in% TRUE,
                  y = function(d) as.numeric(d$breakout75)),
  d60      = list(type = "logit",  rows = function(d) d$value < 60 & d$real_role_next %in% TRUE,
                  y = function(d) as.numeric(d$value_next >= 60 & d$real_role_next %in% TRUE)),
  fall     = list(type = "logit",  rows = function(d) d$value >= 75 & d$real_role_next %in% TRUE,
                  y = function(d) as.numeric(d$value_next < 60)),
  proj     = list(type = "linear", rows = function(d) d$real_role_next %in% TRUE,
                  y = function(d) d$value_next)
)

# blanks that mean "doesn't apply" get a neutral value
prep <- function(d) {
  d <- copy(d)
  for (v in c("value_chg1", "value_chg2")) d[is.na(get(v)), (v) := 0]
  for (v in c("corner3_share_rel", "rim_fg_rel", "mid_fg_rel", "rim_rate_rel", "short_mid_rate_rel",
              "long_mid_rate_rel", "assisted_share_rel", "gmsc36_trend")) d[is.na(get(v)), (v) := 0]
  for (v in c("age", "exp_years", "pick_filled", "height_in", "weight")) {
    m <- median(d[[v]], na.rm = TRUE)
    d[is.na(get(v)), (v) := m]
  }
  for (v in c("undrafted", "college", "traded_midseason")) d[is.na(get(v)), (v) := FALSE]
  d[is.na(changed_team_next), changed_team_next := FALSE]
  d
}

# intercept + inputs, with age squared when age is in (peak-age curve)
design <- function(X, vars, idx = TRUE) {
  M <- cbind(1, X[idx, vars, drop = FALSE])
  if ("age" %in% vars) M <- cbind(M, age_sq = X[idx, "age"]^2)
  M
}

fit <- function(M, y, type) {
  b <- if (type == "logit") suppressWarnings(glm.fit(M, y, family = binomial()))$coefficients else lm.fit(M, y)$coefficients
  b[is.na(b)] <- 0
  b
}

predict_b <- function(M, b, type) {
  z <- M %*% b
  if (type == "logit") as.numeric(1 / (1 + exp(-z))) else as.numeric(pmin(100, pmax(0, z)))
}

auc <- function(p, y) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

logit <- function(p) log(p / (1 - p))
inv_logit <- function(z) 1 / (1 + exp(-z))

# apply a saved Platt correction (from 14_recalibrate.R) if it earned its place
apply_cal <- function(p, cal) {
  if (!cal$use) return(p)
  inv_logit(cal$coef[1] + cal$coef[2] * logit(pmin(pmax(p, 1e-4), 1 - 1e-4)))
}
