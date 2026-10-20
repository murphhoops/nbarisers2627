# First two cuts on the candidate inputs.
#   coverage: present in 90%+ of rows where it applies, and 75%+ in every season
#   overlap:  pairs with |Spearman r| > 0.9 (one of each pair dropped by hand,
#             see the candidate list in fn_model.R)

library(data.table)

d <- readRDS("data/training_table.rds")[season <= 2025 & mp >= 200]

inputs <- c("value", "bpm", "obpm", "dbpm", "multi_est", "scoring", "playmaking", "rebounding",
            "defense_box", "team_context", "value_chg1", "value_chg2", "seasons_on_file", "exp_years",
            "career_peak_value", "mp_prev", "gmsc36_trend", "age", "mp", "mpg", "min_share",
            "starts_share", "games_share", "usg", "position", "role", "ts_pct", "rts",
            "three_rate_box_rel", "tp_pct_shr", "ft_pct_shr", "ft_rate", "ast_pct", "tov_pct",
            "orb_pct", "drb_pct", "stl100", "blk100", "pf100", "rim_rate_rel", "short_mid_rate_rel",
            "long_mid_rate_rel", "corner3_share_rel", "rim_fg_rel", "mid_fg_rel", "assisted_share_rel",
            "pm100", "on_off", "pick_filled", "undrafted", "height_in", "weight", "college",
            "team_adj_net", "traded_midseason", "changed_team_next")

# rows where an input can exist at all (a rookie has no last season, etc.)
applies <- function(v) {
  if (v == "value_chg1") return(!is.na(d$exp_years) & d$exp_years >= 1 & d$season >= 2003)
  if (v == "value_chg2") return(!is.na(d$exp_years) & d$exp_years >= 2 & d$season >= 2004)
  if (v == "changed_team_next") return(d$mp_next > 0)
  if (v == "corner3_share_rel") return(fcoalesce(d$three_rate * d$fga_pbp, 0) >= 25)
  if (v == "rim_fg_rel") return(fcoalesce(as.numeric(d$rim_att), 0) >= 25)
  if (v == "mid_fg_rel") return(fcoalesce(as.numeric(d$mid_att), 0) >= 25)
  rep(TRUE, nrow(d))
}

coverage <- rbindlist(lapply(inputs, function(v) {
  a <- applies(v)
  by_season <- d[a, mean(!is.na(get(v))), by = season]$V1
  data.table(input = v, coverage = mean(!is.na(d[[v]][a])), worst_season = min(by_season))
}))
coverage[, keep := coverage >= 0.90 & worst_season >= 0.75]
print(coverage[order(coverage)])

num <- d[, lapply(.SD, as.numeric), .SDcols = coverage[keep == TRUE, input]]
R <- suppressWarnings(cor(num, method = "spearman", use = "pairwise.complete.obs"))
pairs <- as.data.table(as.table(R))[as.character(V1) < as.character(V2)]
setnames(pairs, c("a", "b", "r"))
print(pairs[abs(r) > 0.9][order(-abs(r))])
