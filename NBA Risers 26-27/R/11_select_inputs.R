# Forward selection for each model. Start from value alone and add the input
# that helps most, as long as it
#   - improves leave-one-season-out log loss by 0.0003 (RMSE by 0.02 for proj), and
#   - doesn't hurt forward-in-time AUC (RMSE for proj): train on seasons before
#     s, test on s, for s = 2016-2025.
# Then drop anything that stopped pulling its weight.

library(data.table)
source("R/fn_model.R")

d <- prep(readRDS("data/training_table.rds")[season <= 2025 & mp >= 200])

select_inputs <- function(name) {
  m <- MODELS[[name]]
  rows <- m$rows(d)
  y <- m$y(d)[rows]
  seasons <- d$season[rows]
  cands <- if (name == "role") setdiff(CANDIDATES, "changed_team_next") else CANDIDATES  # unknown if he didn't play
  X <- scale(as.matrix(d[rows, lapply(.SD, as.numeric), .SDcols = cands]))

  score <- function(vars) {
    M <- design(X, vars)
    pred <- numeric(length(y))
    for (s in unique(seasons)) {
      te <- seasons == s
      pred[te] <- predict_b(M[te, , drop = FALSE], fit(M[!te, , drop = FALSE], y[!te], m$type), m$type)
    }
    fwd <- sapply(2016:2025, function(s) {
      tr <- seasons <= s - 1
      te <- seasons == s
      p <- predict_b(M[te, , drop = FALSE], fit(M[tr, , drop = FALSE], y[tr], m$type), m$type)
      if (m$type == "logit") auc(pmin(pmax(p, 1e-6), 1 - 1e-6), y[te]) else sqrt(mean((y[te] - p)^2))
    })
    if (m$type == "logit") {
      pred <- pmin(pmax(pred, 1e-6), 1 - 1e-6)
      c(loyo = -mean(y * log(pred) + (1 - y) * log(1 - pred)), fwd = mean(fwd))
    } else {
      c(loyo = sqrt(mean((y - pred)^2)), fwd = mean(fwd))
    }
  }
  min_gain <- if (m$type == "logit") 0.0003 else 0.02
  fwd_ok <- function(new, old) if (m$type == "logit") new >= old else new <= old

  sel <- "value"
  cur <- score(sel)
  base <- cur
  repeat {
    res <- rbindlist(lapply(setdiff(cands, sel), function(v) {
      e <- score(c(sel, v))
      data.table(input = v, loyo = e[["loyo"]], fwd = e[["fwd"]])
    }))
    res <- res[cur[["loyo"]] - loyo >= min_gain & fwd_ok(fwd, cur[["fwd"]])][order(loyo)]
    if (!nrow(res)) break
    sel <- c(sel, res$input[1])
    cur <- c(loyo = res$loyo[1], fwd = res$fwd[1])
    cat(sprintf("%s + %-20s loyo %.5f  fwd %.4f\n", name, res$input[1], res$loyo[1], res$fwd[1]))
  }
  repeat {
    if (length(sel) <= 1) break
    drops <- rbindlist(lapply(setdiff(sel, "value"), function(v) {
      e <- score(setdiff(sel, v))
      data.table(input = v, loyo = e[["loyo"]], fwd = e[["fwd"]])
    }))
    w <- drops[order(loyo)][1]
    if (w$loyo - cur[["loyo"]] >= min_gain) break
    sel <- setdiff(sel, w$input)
    cur <- c(loyo = w$loyo, fwd = w$fwd)
    cat(sprintf("%s - %-20s loyo %.5f  fwd %.4f\n", name, w$input, w$loyo, w$fwd))
  }
  list(selected = sel, final = cur, base = base)
}

selected <- lapply(setNames(names(MODELS), names(MODELS)), select_inputs)
saveRDS(selected, "data/selected_inputs.rds")
for (n in names(selected)) {
  cat(sprintf("\n%s: %d inputs, fwd %.4f (value only %.4f)\n  %s\n", n, length(selected[[n]]$selected),
              selected[[n]]$final[["fwd"]], selected[[n]]$base[["fwd"]], paste(selected[[n]]$selected, collapse = ", ")))
}
