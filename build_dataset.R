#!/usr/bin/env Rscript
# =============================================================================
# build_dataset.R  (v2: expanded features, team ratings, COVID exclusion)
#
# Build players.csv for the NBA draft first-round model.
#
# Sources
#   - Barttorvik per-season player stats   barttorvik.com/getadvstats.php?year=YYYY&csv=1
#   - Barttorvik team ratings              barttorvik.com/YYYY_team_results.csv
#   - Basketball-Reference draft tables    basketball-reference.com/draft/NBA_YYYY.html
#
# Output (in `out_dir`)
#   players.csv          one row per player, built from his LAST college season
#   match_report.txt     match rates, sanity checks, picks that need a human
#   manual_matches.csv   template you fill in to fix unmatched picks
#   data_dictionary.csv  one line per column in players.csv
#
# Usage (RStudio, working directory = project folder):
#   source("build_dataset.R")
#   build_dataset(start = 2010, end = 2025, out_dir = "data")
#   build_dataset(start = 2010, end = 2025, out_dir = "data", skip_download = TRUE)
#
# Season / draft alignment: Barttorvik year 2024 = the 2023-24 season; the 2024
# draft is June 2024, so season_year == draft_year.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr)
  library(httr); library(rvest)
})

# -----------------------------------------------------------------------------
# Barttorvik player csv has NO header row. This is the standard mapping; the
# script verifies it against known players after download.
# -----------------------------------------------------------------------------
BT_COLUMNS <- c(
  "player_name", "team", "conf", "GP", "Min_per", "ORtg", "usg", "eFG",
  "TS_per", "ORB_per", "DRB_per", "AST_per", "TO_per", "FTM", "FTA", "FT_per",
  "twoPM", "twoPA", "twoP_per", "TPM", "TPA", "TP_per", "blk_per", "stl_per",
  "ftr", "yr", "ht", "num", "porpag", "adjoe", "pfr", "year", "pid", "type",
  "Rec_Rank", "ast_tov", "rimmade", "rim_att", "midmade", "mid_att",
  "rim_pct", "mid_pct", "dunksmade", "dunk_att", "dunk_pct", "pick", "drtg",
  "adrtg", "dporpag", "stops", "bpm", "obpm", "dbpm", "gbpm", "mp", "ogbpm",
  "dgbpm", "oreb", "dreb", "treb", "ast", "stl", "blk", "pts", "role"
)
BT_NUMERIC <- setdiff(BT_COLUMNS, c("player_name", "team", "conf", "yr", "ht", "num", "pid", "type", "role"))

BT_URL    <- "https://barttorvik.com/getadvstats.php?year=%d&csv=1"
TEAM_URL  <- "https://barttorvik.com/%d_team_results.csv"
BREF_URL  <- "https://www.basketball-reference.com/draft/NBA_%d.html"
UA <- user_agent(paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
                        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"))

# B-Ref school spelling -> Barttorvik spelling. Extend as the report reveals more.
SCHOOL_MAP <- c(
  "UConn" = "Connecticut", "Pitt" = "Pittsburgh", "Ole Miss" = "Mississippi",
  "UNC" = "North Carolina", "Miami (FL)" = "Miami FL", "Miami" = "Miami FL",
  "Loyola (IL)" = "Loyola Chicago", "NC State" = "N.C. State",
  "North Carolina State" = "N.C. State"
)

# Barttorvik `role` -> guard / wing / big
ROLE_MAP <- c(
  "Pure PG" = "guard", "Scoring PG" = "guard", "Combo G" = "guard",
  "Wing G" = "wing", "Wing F" = "wing",
  "Stretch 4" = "big", "PF/C" = "big", "C" = "big"
)

POWER_CONFS <- c("ACC", "B10", "B12", "BE", "SEC", "P12", "P10")

# Draft classes distorted by COVID: the 2019-20 season ended in March 2020 (no
# NCAA tournament) and 2020-21 was shortened with a free eligibility year.
COVID_DRAFT_YEARS <- c(2020, 2021)

# Known #1 overall picks (college players) used to verify labels end to end.
KNOWN_FIRST_ROUNDERS <- tribble(
  ~name,              ~draft_year, ~pick,
  "john wall",         2010, 1,
  "kyrie irving",      2011, 1,
  "anthony davis",     2012, 1,
  "anthony bennett",   2013, 1,
  "andrew wiggins",    2014, 1,
  "karlanthony towns", 2015, 1,
  "ben simmons",       2016, 1,
  "markelle fultz",    2017, 1,
  "deandre ayton",     2018, 1,
  "zion williamson",   2019, 1,
  "anthony edwards",   2020, 1,
  "cade cunningham",   2021, 1,
  "paolo banchero",    2022, 1,
  "cooper flagg",      2025, 1
)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
normalize_name <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- str_to_lower(x)
  x <- str_replace_all(x, "\\b(jr|sr|ii|iii|iv)\\b\\.?", "")
  x <- str_replace_all(x, "[^a-z\\s]", "")
  str_squish(x)
}
normalize_school <- function(x) {
  x <- ifelse(is.na(x), "", str_trim(as.character(x)))
  mapped <- SCHOOL_MAP[x]
  x <- ifelse(is.na(mapped), x, mapped)
  x <- ifelse(str_ends(x, "State"), str_replace(x, "State$", "St."), x)
  str_to_lower(x)
}
height_to_inches <- function(ht) {
  m <- str_match(as.character(ht), "^\\s*(\\d+)\\s*-\\s*(\\d+)\\s*$")
  as.numeric(m[, 2]) * 12 + as.numeric(m[, 3])
}
class_to_num <- function(yr) unname(c(Fr = 1, So = 2, Jr = 3, Sr = 4)[str_trim(as.character(yr))])
clean_names_simple <- function(x) str_replace_all(str_to_lower(str_trim(x)), "[^a-z0-9]+", "_") |> str_replace_all("^_|_$", "")

# -----------------------------------------------------------------------------
# Download
# -----------------------------------------------------------------------------
download_barttorvik <- function(year, raw_dir) {
  out <- file.path(raw_dir, sprintf("barttorvik_%d.csv", year))
  if (file.exists(out) && file.size(out) > 10000) return(out)
  r <- GET(sprintf(BT_URL, year), UA, timeout(60)); stop_for_status(r)
  txt <- content(r, as = "text", encoding = "UTF-8")
  if (nchar(txt) < 10000) stop(sprintf("Barttorvik %d: response too small", year))
  writeLines(txt, out, useBytes = TRUE); out
}

download_team_results <- function(year, raw_dir) {
  out <- file.path(raw_dir, sprintf("teams_%d.csv", year))
  if (file.exists(out) && file.size(out) > 5000) return(out)
  r <- GET(sprintf(TEAM_URL, year), UA, timeout(60)); stop_for_status(r)
  txt <- content(r, as = "text", encoding = "UTF-8")
  if (nchar(txt) < 5000) stop(sprintf("Team results %d: response too small", year))
  writeLines(txt, out, useBytes = TRUE); out
}

download_bref_draft <- function(year, raw_dir) {
  out <- file.path(raw_dir, sprintf("bref_draft_%d.csv", year))
  if (file.exists(out)) return(out)
  r <- GET(sprintf(BREF_URL, year), UA, timeout(60))
  if (status_code(r) == 429) stop("Basketball-Reference rate limit (429). Wait an hour and rerun.")
  stop_for_status(r)
  page <- read_html(content(r, as = "text", encoding = "UTF-8"))
  tbl  <- page |> html_element("table#stats") |> html_table(header = FALSE)
  hdr_row <- which(apply(tbl, 1, function(r) any(r == "Pk")))[1]
  names(tbl) <- make.unique(as.character(unlist(tbl[hdr_row, ])))
  tbl <- tbl[-seq_len(hdr_row), ]
  df <- tbl |>
    transmute(pick = suppressWarnings(as.integer(Pk)), player = Player,
              college = College, team = Tm, draft_year = year) |>
    filter(!is.na(pick))
  write_csv(df, out); Sys.sleep(4); out
}

# -----------------------------------------------------------------------------
# Load
# -----------------------------------------------------------------------------
load_barttorvik <- function(raw_dir, years) {
  bt <- map_dfr(years, function(y) {
    p <- file.path(raw_dir, sprintf("barttorvik_%d.csv", y))
    if (!file.exists(p)) { message("  missing ", basename(p), ", skipping"); return(NULL) }
    df <- read_csv(p, col_names = FALSE, col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
    n <- ncol(df)
    names(df) <- c(BT_COLUMNS[seq_len(min(n, length(BT_COLUMNS)))],
                   if (n > length(BT_COLUMNS)) paste0("extra_", seq_len(n - length(BT_COLUMNS))))
    df$year <- y
    df
  })
  bt |>
    mutate(across(any_of(BT_NUMERIC), ~ suppressWarnings(as.numeric(.x))),
           year       = as.integer(year),
           pid        = as.character(pid),
           name_key   = normalize_name(player_name),
           school_key = normalize_school(team),
           height_in  = height_to_inches(ht),
           class_num  = class_to_num(yr))
}

load_team_results <- function(raw_dir, years) {
  teams <- map_dfr(years, function(y) {
    p <- file.path(raw_dir, sprintf("teams_%d.csv", y))
    if (!file.exists(p)) { message("  missing ", basename(p), ", skipping"); return(NULL) }
    df <- read_csv(p, col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
    names(df) <- clean_names_simple(names(df))
    df$year <- y
    df
  })
  if (nrow(teams) == 0) return(NULL)
  # Find the columns we need by name, defensively.
  pick_col <- function(cands) { hit <- intersect(cands, names(teams)); if (length(hit)) hit[1] else NA }
  c_team <- pick_col(c("team")); c_oe <- pick_col(c("adjoe", "adj_oe", "adjo"))
  c_de <- pick_col(c("adjde", "adj_de", "adjd")); c_bar <- pick_col(c("barthag"))
  c_sos <- pick_col(c("sos", "adj_sos", "elite_sos")); c_wab <- pick_col(c("wab"))
  if (any(is.na(c(c_team, c_oe, c_de)))) {
    message("  team results: could not find team/adjoe/adjde columns. Header was: ", paste(names(teams), collapse = ", "))
    return(NULL)
  }
  teams |> transmute(team = .data[[c_team]], year,
                     team_adjoe = as.numeric(.data[[c_oe]]), team_adjde = as.numeric(.data[[c_de]]),
                     team_adj_em = team_adjoe - team_adjde,
                     team_barthag = if (!is.na(c_bar)) as.numeric(.data[[c_bar]]) else NA_real_,
                     team_sos     = if (!is.na(c_sos)) as.numeric(.data[[c_sos]]) else NA_real_,
                     team_wab     = if (!is.na(c_wab)) as.numeric(.data[[c_wab]]) else NA_real_) |>
    distinct(team, year, .keep_all = TRUE)
}

load_bref <- function(raw_dir, years) {
  map_dfr(years, function(y) {
    p <- file.path(raw_dir, sprintf("bref_draft_%d.csv", y))
    if (!file.exists(p)) { message("  missing ", basename(p), ", skipping"); return(NULL) }
    read_csv(p, col_types = cols(.default = "c"), show_col_types = FALSE)
  }) |>
    mutate(pick = as.integer(pick), draft_year = as.integer(draft_year),
           name_key = normalize_name(player), school_key = normalize_school(college),
           is_college = !is.na(college) & str_trim(college) != "")
}

verify_barttorvik_columns <- function(bt) {
  checks <- tribble(~name, ~year, ~team, ~yr, ~pick,
                    "zion williamson", 2019, "Duke", "Fr", 1,
                    "anthony edwards", 2020, "Georgia", "Fr", 1,
                    "paolo banchero",  2022, "Duke", "Fr", 1)
  out <- c("Column-mapping sanity check (Barttorvik):")
  for (i in seq_len(nrow(checks))) {
    ck <- checks[i, ]; row <- bt |> filter(name_key == ck$name, year == ck$year)
    if (nrow(row) == 0) { out <- c(out, sprintf("  %s %d: not found (fine if year not downloaded)", ck$name, ck$year)); next }
    row <- row[1, ]
    ok <- identical(row$team, ck$team) && identical(row$yr, ck$yr) && isTRUE(row$pick == ck$pick)
    out <- c(out, sprintf("  %s %d: team=%s yr=%s ht=%s pick=%s bpm=%s role=%s -> %s",
                          ck$name, ck$year, row$team, row$yr, row$ht, row$pick, row$bpm, row$role,
                          if (ok) "OK" else "CHECK MAPPING"))
  }
  out
}

# -----------------------------------------------------------------------------
# Match draft picks to Barttorvik player ids
# -----------------------------------------------------------------------------
match_picks <- function(br, bt, manual) {
  manual_lookup <- list()
  if (!is.null(manual) && nrow(manual) > 0) {
    m <- manual |> filter(!is.na(bt_pid), str_trim(as.character(bt_pid)) != "")
    for (i in seq_len(nrow(m)))
      manual_lookup[[paste(normalize_name(m$bref_player[i]), m$draft_year[i])]] <- str_trim(as.character(m$bt_pid[i]))
  }
  pid <- rep(NA_character_, nrow(br)); how <- rep("", nrow(br)); report <- character()
  for (i in seq_len(nrow(br))) {
    r <- br[i, ]; key <- paste(r$name_key, r$draft_year)
    if (!is.null(manual_lookup[[key]])) { pid[i] <- manual_lookup[[key]]; how[i] <- "manual"; next }
    if (!r$is_college) { how[i] <- "non_college"; next }
    last_name <- str_extract(r$name_key, "\\S+$")
    same_year <- bt |> filter(year == r$draft_year, name_key == r$name_key)
    if (nrow(same_year) == 1) { pid[i] <- same_year$pid; how[i] <- "name+year"; next }
    if (nrow(same_year) > 1) {
      by_school <- same_year |> filter(school_key == r$school_key)
      if (nrow(by_school) == 1) { pid[i] <- by_school$pid; how[i] <- "name+year+school"; next }
      by_pick <- same_year |> filter(!is.na(pick), pick == r$pick)
      if (nrow(by_pick) == 1) { pid[i] <- by_pick$pid; how[i] <- "name+year+btpick"; next }
      how[i] <- "ambiguous"
      report <- c(report, sprintf("AMBIGUOUS  %d #%2d %s (%s): %s", r$draft_year, r$pick, r$player, r$college,
                                  paste(sprintf("%s/%s/pid=%s", same_year$player_name, same_year$team, same_year$pid), collapse = "; ")))
      next
    }

    # --- Fallback A: nickname / different first name (Bam vs Edrice Adebayo).
    # Barttorvik records the pick number itself; if exactly one player in that
    # season carries this pick AND shares the last name or the school, accept.
    by_pick <- bt |> filter(year == r$draft_year, !is.na(pick), pick == r$pick)
    if (nrow(by_pick) == 1 && !is.na(last_name) &&
        (str_detect(by_pick$name_key, paste0("\\b", last_name, "\\b")) || by_pick$school_key == r$school_key)) {
      pid[i] <- by_pick$pid; how[i] <- "btpick+lastname/school"
      report <- c(report, sprintf("VERIFIED   %d #%2d B-Ref '%s' (%s) = Barttorvik '%s' (%s, %s, pick %d)",
                                  r$draft_year, r$pick, r$player, r$college, by_pick$player_name, by_pick$team, by_pick$yr, by_pick$pick))
      next
    }
    # --- Fallback B: same last name + same school + same season, unique.
    by_last <- bt |> filter(year == r$draft_year, school_key == r$school_key,
                            str_detect(name_key, paste0("\\b", last_name, "$")))
    if (nrow(by_last) == 1 && !is.na(last_name) && nchar(last_name) >= 3) {
      pid[i] <- by_last$pid; how[i] <- "lastname+school"
      report <- c(report, sprintf("VERIFIED   %d #%2d B-Ref '%s' (%s) = Barttorvik '%s' (%s, %s, bt pick %s)  <- eyeball this one",
                                  r$draft_year, r$pick, r$player, r$college, by_last$player_name, by_last$team, by_last$yr,
                                  ifelse(is.na(by_last$pick), "none", by_last$pick)))
      next
    }

    prev <- bt |> filter(year == r$draft_year - 1, name_key == r$name_key)
    if (nrow(prev) == 1) {
      pid[i] <- prev$pid; how[i] <- "name+prev_year"
      report <- c(report, sprintf("PREV_YEAR  %d #%2d %s (%s) matched to %d season (sat out?)", r$draft_year, r$pick, r$player, r$college, r$draft_year - 1))
      next
    }
    cand <- bt |> filter(year == r$draft_year, school_key == r$school_key)
    last <- str_extract(r$name_key, "\\S+$"); sugg <- ""
    if (nrow(cand) > 0 && !is.na(last) && last != "") {
      c2 <- cand |> filter(str_detect(name_key, fixed(last)))
      if (nrow(c2) > 0) sugg <- paste0(" | suggestions: ", paste(sprintf("%s pid=%s", c2$player_name, c2$pid), collapse = "; "))
    }
    how[i] <- "unmatched"
    report <- c(report, sprintf("UNMATCHED  %d #%2d %s (%s)%s", r$draft_year, r$pick, r$player, r$college, sugg))
  }
  br$pid <- pid; br$match_how <- how
  list(br = br, report = report)
}

# -----------------------------------------------------------------------------
# Build one row per player
# -----------------------------------------------------------------------------
build_players <- function(bt, br, teams, start, end, min_minutes, exclude_years) {
  mp_is_per_game <- max(bt$mp, na.rm = TRUE) <= 60        # Barttorvik mp = minutes per game
  bt <- bt |> arrange(pid, year) |>
    mutate(mp_total = if (mp_is_per_game) GP * mp else mp,
           mpg      = if (mp_is_per_game) mp else mp / GP)

  # recruiting rank: 0 / NA = unranked
  ranked <- bt$Rec_Rank[!is.na(bt$Rec_Rank) & bt$Rec_Rank > 0]
  unranked_value <- if (length(ranked)) max(ranked) + 1 else 101

  last <- bt |> group_by(pid) |>
    mutate(seasons_played = n(), season_idx = row_number(),
           prev_bpm = lag(bpm), prev_usg = lag(usg), prev_mpg = lag(mpg), prev_TS_per = lag(TS_per),
           prev_team = lag(team)) |>
    filter(season_idx == n()) |> ungroup() |>
    mutate(
      has_prior_season = as.integer(!is.na(prev_bpm)),
      delta_bpm = bpm - prev_bpm, delta_usg = usg - prev_usg,
      delta_mpg = mpg - prev_mpg, delta_TS_per = TS_per - prev_TS_per,
      transfer  = as.integer(!is.na(prev_team) & prev_team != team),
      # position
      position  = unname(ROLE_MAP[str_trim(role)]),
      pos_guard = as.integer(position == "guard"), pos_wing = as.integer(position == "wing"),
      pos_big   = as.integer(position == "big"),
      # recruiting
      unranked     = as.integer(is.na(Rec_Rank) | Rec_Rank == 0),
      recruit_rank = ifelse(unranked == 1, unranked_value, Rec_Rank),
      recruit_tier = case_when(unranked == 1 ~ "unranked", Rec_Rank <= 10 ~ "top10",
                               Rec_Rank <= 30 ~ "top30", Rec_Rank <= 100 ~ "top100", TRUE ~ "ranked100plus"),
      # shooting volume / accuracy
      FGA        = twoPA + TPA,
      TPA_rate   = TPA / na_if(FGA, 0),
      TPA_per40  = TPA / na_if(GP * mpg, 0) * 40,
      TP_per_shrunk = (TPM + 0.33 * 50) / (TPA + 50) * 100,    # pulls low-volume shooters toward 33%
      TP_per_min30  = ifelse(TPA >= 30, TP_per, NA_real_),
      # per-40 production (pts/ast/treb/stl/blk are per game in Barttorvik)
      pts_per40  = pts  / na_if(mpg, 0) * 40,
      ast_per40  = ast  / na_if(mpg, 0) * 40,
      treb_per40 = treb / na_if(mpg, 0) * 40,
      stl_per40  = stl  / na_if(mpg, 0) * 40,
      blk_per40  = blk  / na_if(mpg, 0) * 40,
      stocks_per = stl_per + blk_per,
      dunks_per40 = dunksmade / na_if(GP * mpg, 0) * 40,
      rim_rate   = rim_att / na_if(FGA, 0),
      power_conf = as.integer(conf %in% POWER_CONFS)
    ) |>
    # Height relative to the player's position in that season. A 6'8" guard is
    # tall; a 6'8" center is small. Use this in the model instead of raw height.
    group_by(position, year) |>
    mutate(height_vs_pos = height_in - mean(height_in, na.rm = TRUE),
           height_z_pos  = height_vs_pos / sd(height_in, na.rm = TRUE)) |>
    ungroup()

  career <- bt |> group_by(pid) |>
    summarise(career_FTA = sum(FTA, na.rm = TRUE), career_FT_pct = sum(FTM, na.rm = TRUE) / na_if(sum(FTA, na.rm = TRUE), 0) * 100,
              career_TPA = sum(TPA, na.rm = TRUE), career_TP_pct = sum(TPM, na.rm = TRUE) / na_if(sum(TPA, na.rm = TRUE), 0) * 100,
              career_mp  = sum(mp_total, na.rm = TRUE), .groups = "drop")

  picks <- br |> filter(!is.na(pid)) |>
    transmute(pid, bref_draft_year = draft_year, pick_number = pick, nba_team = team) |>
    distinct(pid, .keep_all = TRUE)

  last <- last |>
    rename(bt_pick = pick) |>
    left_join(career, by = "pid") |>
    left_join(picks,  by = "pid")
  if (!is.null(teams)) last <- last |> left_join(teams, by = c("team", "year"))

  last <- last |>
    mutate(drafted     = as.integer(!is.na(pick_number)),
           first_round = as.integer(!is.na(pick_number) & pick_number <= 30),
           draft_status = case_when(is.na(pick_number) ~ "Undrafted", pick_number <= 30 ~ "Round 1", TRUE ~ "Round 2"),
           pick_label   = ifelse(is.na(pick_number), "Undrafted", as.character(pick_number)),
           nba_team     = coalesce(nba_team, "Undrafted"),
           # a player who sat out his draft year keeps his real draft class
           draft_year  = as.integer(coalesce(bref_draft_year, year)),
           draft_class_mismatch = as.integer(!is.na(bref_draft_year) & bref_draft_year != year),
           season_year = year)

  pool <- last |>
    filter(draft_year >= start, draft_year <= end,                 # <- fixes the 2009 (Blake Griffin) leak
           (!is.na(mp_total) & mp_total >= min_minutes) | drafted == 1,
           !(draft_year %in% exclude_years))

  cols <- c("pid", "player_name", "team", "conf", "power_conf", "draft_year", "season_year",
            "yr", "class_num", "seasons_played", "has_prior_season", "transfer",
            "ht", "height_in", "height_vs_pos", "height_z_pos", "role", "position", "pos_guard", "pos_wing", "pos_big",
            "Rec_Rank", "recruit_rank", "unranked", "recruit_tier",
            "GP", "Min_per", "mpg", "mp_total",
            "usg", "ORtg", "eFG", "TS_per", "pts_per40",
            "FT_per", "FTA", "ftr", "TPA", "TPA_rate", "TPA_per40", "TP_per", "TP_per_shrunk", "TP_per_min30",
            "AST_per", "TO_per", "ast_tov", "ast_per40",
            "ORB_per", "DRB_per", "treb_per40", "stl_per", "blk_per", "stocks_per", "stl_per40", "blk_per40",
            "rim_rate", "rim_pct", "mid_pct", "dunksmade", "dunks_per40", "pfr",
            "bpm", "obpm", "dbpm", "gbpm", "ogbpm", "dgbpm", "porpag", "dporpag", "adjoe", "drtg", "adrtg", "stops",
            "team_adjoe", "team_adjde", "team_adj_em", "team_barthag", "team_sos", "team_wab",
            "prev_bpm", "prev_usg", "prev_mpg", "prev_TS_per", "delta_bpm", "delta_usg", "delta_mpg", "delta_TS_per",
            "career_FTA", "career_FT_pct", "career_TPA", "career_TP_pct", "career_mp",
            "drafted", "first_round", "draft_status", "pick_number", "pick_label", "bt_pick", "nba_team", "bref_draft_year", "draft_class_mismatch")
  pool |> select(any_of(cols)) |>
    rename(school = team, class_year = yr) |>
    arrange(draft_year, pick_number, player_name)
}

DATA_DICTIONARY <- tribble(
  ~column, ~description,
  "pid", "Barttorvik player id (join key)",
  "draft_year", "Draft class the row is labeled against (= final season year, or the real draft year if he sat out)",
  "season_year", "Year of the final college season used for the stats",
  "class_num", "Fr=1, So=2, Jr=3, Sr=4 (age proxy)",
  "seasons_played", "College seasons in the data (within downloaded range)",
  "transfer", "1 if final-season school differs from previous-season school",
  "height_in", "Listed height in inches",
  "height_vs_pos", "Height minus the average height of players at the same position in the same season (inches)",
  "height_z_pos", "Same, in standard deviations",
  "position / pos_*", "guard / wing / big, mapped from Barttorvik role; pos_guard is the reference category",
  "Rec_Rank", "Barttorvik recruiting rank (raw; 0/NA = unranked)",
  "recruit_rank", "Rec_Rank with unranked set to (max rank + 1)",
  "unranked", "1 if no recruiting rank",
  "recruit_tier", "top10 / top30 / top100 / ranked100plus / unranked",
  "GP, Min_per, mpg, mp_total", "Games, % of team minutes, minutes per game, total minutes",
  "usg", "Usage rate",
  "ORtg", "Offensive rating (points produced per 100 possessions)",
  "eFG, TS_per", "Effective FG%, true shooting %",
  "pts_per40", "Points per 40 minutes",
  "FT_per, FTA, ftr", "FT%, FT attempts, FT rate (FTA/FGA)",
  "TPA, TPA_rate, TPA_per40", "3PA total, share of FGA that are 3s, 3PA per 40",
  "TP_per", "Raw 3P%",
  "TP_per_shrunk", "3P% shrunk toward 33% with 50 pseudo-attempts (use this instead of raw)",
  "TP_per_min30", "3P% only when TPA >= 30, else NA",
  "AST_per, TO_per, ast_tov, ast_per40", "Assist rate, turnover rate, A/TO, assists per 40",
  "ORB_per, DRB_per, treb_per40", "Rebounding rates, rebounds per 40",
  "stl_per, blk_per, stocks_per", "Steal %, block %, their sum",
  "rim_rate, rim_pct, mid_pct", "Share of FGA at rim, rim FG%, midrange FG%",
  "dunksmade, dunks_per40", "Dunks (athleticism proxy)",
  "pfr", "Personal foul rate",
  "bpm, obpm, dbpm", "Box plus-minus and its offensive/defensive parts",
  "gbpm, ogbpm, dgbpm", "Game-based BPM (Barttorvik's play-by-play version)",
  "porpag, dporpag", "Points over replacement per adjusted game, offensive / defensive (value metrics)",
  "adjoe", "Player adjusted offensive efficiency",
  "drtg, adrtg, stops", "Defensive rating, adjusted defensive rating, stops (team-dependent)",
  "team_adjoe, team_adjde, team_adj_em", "Team adjusted O / D efficiency and margin (competition level)",
  "team_barthag, team_sos, team_wab", "Team win probability vs average, strength of schedule, wins above bubble",
  "prev_*, delta_*", "Previous-season value and change (NA for one-season players)",
  "career_*", "All-seasons totals/percentages (FT and 3P)",
  "drafted, first_round, pick_number", "Labels: drafted anywhere, picked 1-30, pick number (blank = undrafted; keep numeric for modeling)",
  "draft_status, pick_label", "Readable versions: 'Round 1' / 'Round 2' / 'Undrafted', and the pick number or 'Undrafted'",
  "nba_team", "Drafting team, or 'Undrafted'",
  "bt_pick", "Barttorvik's own pick column (cross-check only, do not use as a feature)",
  "draft_class_mismatch", "1 if B-Ref draft year != final season year (sat out)"
)

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------
build_dataset <- function(start = 2010, end = 2025, out_dir = "data", min_minutes = 400,
                          exclude_years = COVID_DRAFT_YEARS, skip_download = FALSE) {
  raw_dir <- file.path(out_dir, "raw"); dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  # one extra early season (prev-season features) and one extra late season (so
  # undrafted players from the last class who stayed in school are recognised)
  years <- (start - 1):(end + 1)

  if (!skip_download) {
    cat("Downloading Barttorvik player seasons...\n")
    for (y in years) tryCatch({ download_barttorvik(y, raw_dir); cat("  ", y, "ok\n") },
                              error = function(e) message("  ", y, " FAILED: ", conditionMessage(e)))
    cat("Downloading Barttorvik team ratings...\n")
    for (y in start:end) tryCatch({ download_team_results(y, raw_dir); cat("  ", y, "ok\n") },
                                  error = function(e) message("  ", y, " FAILED: ", conditionMessage(e)))
    cat("Downloading Basketball-Reference draft tables...\n")
    for (y in start:end) tryCatch({ download_bref_draft(y, raw_dir); cat("  ", y, "ok\n") },
                                  error = function(e) message("  ", y, " FAILED: ", conditionMessage(e)))
  }

  cat("Loading...\n")
  bt    <- load_barttorvik(raw_dir, years)
  teams <- load_team_results(raw_dir, start:end)
  br    <- load_bref(raw_dir, start:end)
  mapping_lines <- verify_barttorvik_columns(bt)
  cat(paste(mapping_lines, collapse = "\n"), "\n")

  manual_path <- file.path(out_dir, "manual_matches.csv")
  manual <- if (file.exists(manual_path)) read_csv(manual_path, col_types = cols(.default = "c"), show_col_types = FALSE)
            else tibble(bref_player = character(), draft_year = character(), college = character(), bt_pid = character(), note = character())

  res <- match_picks(br, bt, manual); br <- res$br; report <- res$report

  chk <- br |> filter(!is.na(pid)) |> left_join(bt |> select(pid, year, pick_bt = pick), by = c("pid", "draft_year" = "year"))
  disagree <- chk |> filter(!is.na(pick_bt), pick != pick_bt)

  needs_manual <- br |> filter(match_how %in% c("unmatched", "ambiguous"))
  existing <- paste(normalize_name(manual$bref_player), manual$draft_year)
  new_rows <- needs_manual |> filter(!(paste(name_key, draft_year) %in% existing)) |>
    transmute(bref_player = player, draft_year = as.character(draft_year), college, bt_pid = "", note = match_how)
  manual <- bind_rows(manual, new_rows); write_csv(manual, manual_path, na = "")

  players <- build_players(bt, br, teams, start, end, min_minutes, exclude_years)
  write_csv(players, file.path(out_dir, "players.csv"), na = "")
  write_csv(DATA_DICTIONARY, file.path(out_dir, "data_dictionary.csv"))

  # ---------------- label verification ----------------
  kept_years <- setdiff(start:end, exclude_years)
  matched_fr <- br |> filter(is_college, pick <= 30, !is.na(pid), draft_year %in% kept_years)
  in_players <- players |> filter(first_round == 1)
  label_check <- c(
    "Label verification:",
    sprintf("  matched college 1st-rounders in kept years: %d ; rows with first_round=1 in players.csv: %d -> %s",
            nrow(matched_fr), nrow(in_players), if (nrow(matched_fr) == nrow(in_players)) "OK" else "MISMATCH"),
    sprintf("  rows outside draft years %d-%d: %d (must be 0)", start, end,
            sum(players$draft_year < start | players$draft_year > end)),
    sprintf("  rows in excluded years (%s): %d (must be 0)", paste(exclude_years, collapse = ","),
            sum(players$draft_year %in% exclude_years))
  )
  known <- KNOWN_FIRST_ROUNDERS |> filter(draft_year %in% kept_years)
  for (i in seq_len(nrow(known))) {
    k <- known[i, ]
    row <- players |> filter(normalize_name(player_name) == k$name, draft_year == k$draft_year)
    label_check <- c(label_check,
      if (nrow(row) == 0) sprintf("  %-18s %d: NOT IN players.csv  <- check", k$name, k$draft_year)
      else sprintf("  %-18s %d: first_round=%d pick=%s -> %s", k$name, k$draft_year, row$first_round[1], row$pick_number[1],
                   if (row$first_round[1] == 1 && isTRUE(row$pick_number[1] == k$pick)) "OK" else "WRONG"))
  }
  # The famous leak: Blake Griffin (last season 2009) must NOT be in the pool
  griffin <- players |> filter(normalize_name(player_name) == "blake griffin")
  label_check <- c(label_check, sprintf("  blake griffin (2009 class) rows in players.csv: %d (must be 0)", nrow(griffin)))

  # ---------------- coverage of new features ----------------
  team_join_rate <- if ("team_adj_em" %in% names(players)) mean(!is.na(players$team_adj_em)) else NA
  cov <- c("Feature coverage in players.csv:",
           sprintf("  team ratings joined: %.1f%% of rows", 100 * team_join_rate),
           sprintf("  position mapped:     %.1f%% of rows  (roles seen: %s)", 100 * mean(!is.na(players$position)),
                   paste(names(sort(table(players$role), decreasing = TRUE)), collapse = ", ")),
           sprintf("  recruiting rank:     %.1f%% ranked (%d first-rounders unranked)", 100 * mean(players$unranked == 0),
                   sum(players$unranked == 1 & players$first_round == 1)),
           sprintf("  transfers:           %d rows flagged", sum(players$transfer, na.rm = TRUE)))

  college <- br |> filter(is_college); fr <- college |> filter(pick <= 30)
  per_year <- players |> group_by(draft_year) |> summarise(rows = n(), first_rounders = sum(first_round), drafted = sum(drafted), .groups = "drop")
  lines <- c(
    sprintf("Draft years %d-%d, excluding %s", start, end, paste(exclude_years, collapse = ", ")),
    sprintf("Total picks: %d   college picks: %d   college 1st-rounders: %d", nrow(br), nrow(college), nrow(fr)),
    "", mapping_lines,
    "", "Match rate by group:",
    sprintf("  %-20s %4d/%-4d = %5.1f%%", "college picks", sum(!is.na(college$pid)), nrow(college), 100 * mean(!is.na(college$pid))),
    sprintf("  %-20s %4d/%-4d = %5.1f%%", "college 1st round", sum(!is.na(fr$pid)), nrow(fr), 100 * mean(!is.na(fr$pid))),
    "", "Match method counts:", capture.output(print(table(br$match_how))),
    "", sprintf("Picks where B-Ref pick != Barttorvik 'pick' column: %d", nrow(disagree)),
    if (nrow(disagree) > 0) sprintf("   %d %s: bref=%d bt=%d", head(disagree$draft_year, 20), head(disagree$player, 20), head(disagree$pick, 20), head(disagree$pick_bt, 20)),
    "", label_check, "", cov,
    "", sprintf("players.csv: %d rows, %d first-rounders, %d drafted, %d undrafted",
                nrow(players), sum(players$first_round), sum(players$drafted), sum(players$drafted == 0)),
    "", "Rows per draft year:", capture.output(print(as.data.frame(per_year), row.names = FALSE)),
    "", "Picks needing attention (also listed in manual_matches.csv):", paste0("  ", report)
  )
  writeLines(lines, file.path(out_dir, "match_report.txt"))
  cat(paste(lines[seq_len(min(length(lines), 45))], collapse = "\n"), "\n")
  cat(sprintf("\nWrote %s, %s, %s, %s\n", file.path(out_dir, "players.csv"), file.path(out_dir, "match_report.txt"),
              manual_path, file.path(out_dir, "data_dictionary.csv")))
  invisible(players)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2)
    build_dataset(start = as.integer(args[1]), end = as.integer(args[2]),
                  out_dir = if (length(args) >= 3) args[3] else "data",
                  skip_download = length(args) >= 4 && args[4] == "skip")
}
