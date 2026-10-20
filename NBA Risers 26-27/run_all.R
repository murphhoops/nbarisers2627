# Runs the whole pipeline from the repo root.
# Needs: data.table, jsonlite, rvest, xml2, ranger
# The Basketball-Reference scrape is slow on purpose (one page every 4 seconds),
# and 11_select_inputs.R takes a while.

steps <- c(
  "00_download.R",       # ESPN box scores, play-by-play, rosters
  "01_scrape_bbref.R",   # ages, bios, draft picks, BBRef BPM
  "02_seasons.R",        # player stints and team seasons
  "03_avg_lead.R",       # average lead from play-by-play
  "04_bpm.R",            # BPM and its parts
  "05_shrinkage.R",      # how hard to shrink small samples
  "06_value_score.R",    # 0-100 score
  "07_link_bbref.R",     # match to BBRef, check our BPM
  "08_shots_trend.R",    # shot profile, in-season trend
  "09_training_table.R", # inputs and next-season targets
  "10_screen.R",         # coverage and overlap checks
  "11_select_inputs.R",  # forward selection for each model
  "12_benchmark.R",      # random forest comparison
  "13_forward.R",        # forward-in-time predictions
  "14_recalibrate.R",    # Platt scaling where it helps
  "15_backtest.R",       # past lists
  "16_predict.R",        # 2026-27
  "17_board.R"           # docs/index.html
)

for (s in steps) {
  message("running ", s)
  source(file.path("R", s), local = new.env())
}
