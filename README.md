# NBA Risers Board

Three lists for the 2026-27 season, built in R:

- **Breakout Candidates** - young players most likely to have a top-quarter season (score 75+)
- **Diamond Candidates** - players 27 or younger, currently below 50, most likely to become useful (60+)
- **Regression Watch** - current top-quarter players most likely to fall below 60

The board is a single HTML page in `docs/index.html`. All predictions are in `output/predictions_2026_27.csv`.

## The score

Every player-season gets a 0-100 value score:

1. **BPM, rebuilt from box scores.** I used the published BPM 2.0 formula from Basketball-Reference on ESPN box scores going back to 2001-02, with team ratings adjusted for schedule and average lead (from play-by-play). Across all 25 seasons it lines up with Basketball-Reference's BPM at r = 0.995, with a mean absolute difference of about 0.2 points.
2. **Small samples are shrunk** toward what players at that minutes level usually post. The shrinkage strength was picked by which version best predicted the next season.
3. **Ranked within the season** against players with 500+ minutes. A 75 means top quarter of that season.

Each player's BPM is split into scoring, playmaking, rebounding, box-score defense and team context, measured against the league average, so the board can show where the number comes from.

## The models

Each 2002-2025 player-season is paired with that player's next season. Five logistic/linear models:

| Model | Question |
|---|---|
| Real role | Will he play 1,000+ minutes (scaled for short seasons)? |
| Breakout % | If he gets a real role, will he score 75+? |
| Chance of 60+ | If he gets a real role, will he score 60+? |
| Chance of falling | For a current 75+, if he keeps a real role, will he drop below 60? |
| Projected score | If he gets a real role, what will he score? (with an 80% range) |

Role and production are kept separate on purpose: "good if he plays" and "likely to play" are different questions, and the board shows both.

**Picking inputs.** I started with about 60 candidate inputs (score history, age, experience, usage, shooting, rebounding, shot locations from play-by-play, plus-minus, draft pick, height, team context). After dropping ones with poor coverage or near-duplicates, 52 went into forward selection. An input only made it in if it improved leave-one-season-out log loss by at least 0.0003 and didn't lower forward-in-time AUC (train on past seasons only, test on 2016-2025). Each model ended up with 7-12 inputs. Age and a three-season BPM average matter in every model.

A random forest given all 52 inputs did not beat the small models on the same forward test, so I kept the simple, explainable versions.

**Calibration.** Probabilities were checked against what actually happened in forward-in-time predictions. Platt scaling was applied to the real-role and Breakout models, where it helped on held-out seasons, and left off the other two, where it didn't.

## How the lists did in the past

Lists built for each season from 2016 to 2025, using only models trained on earlier seasons:

| List | Result |
|---|---|
| Breakout, top 10 | 45 of 100 reached 75+ with a real role (9% of the pool did). 45 of the 83 who got a real role made it. |
| Diamond, top 25 | 28% reached 60+ with a real role, vs 6% of the rest of the pool. 15% of those who played 500+ minutes reached 75. |
| Regression Watch, top 10 | 41% of those who kept a real role fell below 60, vs 4% of the rest of the top-quarter pool. Another 25% lost the role. |

## Limits

These are odds, not predictions of what will happen to a specific player. The models can't see injuries, coaching changes, contract situations or crowded rotations, and the projected score has a wide range (about plus or minus 22 points). Box-score defense misses a lot of real defense. Team moves come from ESPN's 2026-27 rosters at the time of the data pull.

## Running it

From the repo root:

```r
source("run_all.R")
```

Packages: `data.table`, `jsonlite`, `rvest`, `xml2`, `ranger`.

`00_download.R` pulls ESPN data from the [sportsdataverse](https://github.com/sportsdataverse/sportsdataverse-data) releases. `01_scrape_bbref.R` reads Basketball-Reference pages (one every 4 seconds, cached in `data/raw/bbref`). The `data/` folder isn't tracked; everything in it is rebuilt by the scripts.

## Data

- ESPN box scores, play-by-play and rosters via sportsdataverse
- Basketball-Reference for ages, player bios, draft picks and their BPM (used to check mine)
