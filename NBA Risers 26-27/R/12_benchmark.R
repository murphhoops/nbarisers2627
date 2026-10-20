# Sanity check: a random forest with all 52 inputs, same forward-in-time test.
# If it beat the small models by a lot, they'd be missing something.

library(data.table)
library(ranger)
source("R/fn_model.R")

d <- prep(readRDS("data/training_table.rds")[season <= 2025 & mp >= 200])
selected <- readRDS("data/selected_inputs.rds")

out <- rbindlist(lapply(c("role", "breakout", "proj"), function(name) {
  m <- MODELS[[name]]
  rows <- m$rows(d)
  y <- m$y(d)[rows]
  seasons <- d$season[rows]
  cands <- if (name == "role") setdiff(CANDIDATES, "changed_team_next") else CANDIDATES
  X <- scale(as.matrix(d[rows, lapply(.SD, as.numeric), .SDcols = cands]))
  rf <- sapply(2016:2025, function(s) {
    tr <- seasons <= s - 1
    te <- seasons == s
    f <- ranger(x = X[tr, ], y = if (m$type == "logit") factor(y[tr]) else y[tr],
                probability = m$type == "logit", num.trees = 500, min.node.size = 25,
                num.threads = 2, seed = 1)
    p <- predict(f, X[te, ])$predictions
    if (m$type == "logit") auc(p[, "1"], y[te]) else sqrt(mean((y[te] - p)^2))
  })
  data.table(model = name, metric = if (m$type == "logit") "forward AUC" else "forward RMSE",
             small_model = round(selected[[name]]$final[["fwd"]], 3), random_forest = round(mean(rf), 3))
}))
print(out)
saveRDS(out, "data/benchmark.rds")
