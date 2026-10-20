# Build each season's three lists from the forward predictions (2016-2025)
# and see what happened the next year.

library(data.table)
source("R/fn_model.R")

f <- readRDS("data/forward_predictions.rds")
cal <- readRDS("data/calibration.rds")
slen <- readRDS("data/team_seasons.rds")[, .(len = max(g) / 82), by = season]
f <- merge(f, slen, by = "season")
f[, p_role := apply_cal(p_role, cal$role)]
f[, p_breakout := apply_cal(p_breakout, cal$breakout)]
f[, p_d60 := apply_cal(p_d60, cal$d60)]
f[, p_fall := apply_cal(p_fall, cal$fall)]
f[, p_60 := p_d60 * p_role]
f[, role := real_role_next %in% TRUE]
f[, played := mp_next >= 500]

top_n <- function(x, col, n) {
  x <- x[order(-get(col))]
  x[, top := seq_len(.N) <= n]
  x
}

# Breakout: 4 or fewer years in, 300+ min, at or below 75; top 10 by Breakout %
brk <- rbindlist(lapply(2016:2025, function(s) top_n(f[season == s & exp_years <= 4 & mp >= 300 & value <= 75], "p_breakout", 10)))
breakout <- list(
  top10_hits = brk[top == TRUE, sum(breakout75 %in% TRUE)],
  top10_role = brk[top == TRUE, sum(role)],
  pool_rate = brk[, mean(breakout75 %in% TRUE), by = season][, mean(V1)])

# Diamond: 27 or younger, below 50; top 25 by chance of 60+ with a real role
dia <- rbindlist(lapply(2016:2025, function(s) top_n(f[season == s & value < 50 & !is.na(age) & age <= 27], "p_60", 25)))
dia[, hit60 := role & value_next >= 60]
diamond <- list(
  top25_rate = dia[top == TRUE, mean(hit60)],
  rest_rate = dia[top == FALSE, mean(hit60)],
  top25_reach75 = dia[top == TRUE & played == TRUE, mean(value_next >= 75)])

# Regression Watch: 75+ with a real role; top 10 by chance of falling below 60
fal <- rbindlist(lapply(2016:2025, function(s) {
  x <- f[season == s & value >= 75 & mp >= 1000 * len][order(-p_fall)]
  x[, rank := seq_len(.N)]
}))
fal[, fell := role & value_next < 60]
fall <- list(
  top10_fell_if_role = fal[rank <= 10 & role == TRUE, mean(fell)],
  rest_fell_if_role = fal[rank > 25 & role == TRUE, mean(fell)],   # outside the 25 shown
  top10_lost_role = fal[rank <= 10, mean(!role)])

str(list(breakout = breakout, diamond = diamond, fall = fall))
saveRDS(list(breakout = breakout, diamond = diamond, fall = fall), "data/backtest.rds")
