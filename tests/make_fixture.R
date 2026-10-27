# Generate small fake Barttorvik + B-Ref raw files so the pipeline can be tested offline.
# Usage:  source("build_dataset.R"); source("tests/make_fixture.R"); make_fixture("tests/fixture")
#         build_dataset(2017, 2019, out_dir = "tests/fixture", skip_download = TRUE, exclude_years = 2018)
suppressPackageStartupMessages({ library(dplyr); library(readr) })

make_fixture <- function(out_dir) {
  if (!exists("BT_COLUMNS")) source("build_dataset.R")
  raw <- file.path(out_dir, "raw"); dir.create(raw, recursive = TRUE, showWarnings = FALSE)
  set.seed(1)
  YEARS   <- 2016:2020                         # 2016 = extra early season, 2020 = extra late season
  DRAFTS  <- 2017:2019
  SCHOOLS <- c("Duke", "Kentucky", "Connecticut", "Pittsburgh", "Miami FL", "Gonzaga", "Kansas", "N.C. State")
  ROLES   <- c("Pure PG", "Scoring PG", "Combo G", "Wing G", "Wing F", "Stretch 4", "PF/C", "C")
  firsts  <- c("Zion", "Ja", "RJ", "De'Andre", "Cam", "Jarrett", "Coby", "Tyler", "Nassir", "Rui", "Bol", "Romeo")
  lasts   <- c("Williamson", "Morant", "Barrett", "Hunter", "Reddish", "Culver", "White", "Herro", "Little", "Hachimura", "Bol", "Langford")

  players <- tibble(
    pid = 1001:1300,
    name = paste(sample(firsts, 300, TRUE), sample(lasts, 300, TRUE), 1:300),
    school = sample(SCHOOLS, 300, TRUE),
    start = sample(2016:2019, 300, TRUE),
    n = sample(c(1, 1, 2, 3, 4), 300, TRUE),
    quality = rnorm(300),
    rec = ifelse(runif(300) < 0.3, sample(1:100, 300, TRUE), 0)
  ) |> bind_rows(
    tibble(pid = 9001, name = "Same Name",  school = "Duke",     start = 2019, n = 1, quality = 2.5, rec = 1),
    tibble(pid = 9002, name = "Same Name",  school = "Kentucky", start = 2019, n = 1, quality = -1,  rec = 0),
    tibble(pid = 9003, name = "Gary Trent", school = "Duke",     start = 2018, n = 1, quality = 2,   rec = 15),
    # nickname case: Barttorvik says Edrice, B-Ref says Bam
    tibble(pid = 9006, name = "Edrice Adebayo", school = "Kentucky", start = 2017, n = 1, quality = 3.5, rec = 5),
    # Blake Griffin case: great player whose last season is the extra EARLY year -> must not appear
    tibble(pid = 9004, name = "Blake Griffin", school = "Kansas", start = 2016, n = 1, quality = 4, rec = 20),
    # transfer: two schools
    tibble(pid = 9005, name = "Transfer Guy", school = "Pittsburgh", start = 2017, n = 2, quality = 1.5, rec = 0)
  )

  seasons <- players |> rowwise() |> reframe(
    pid = pid, player_name = name, team = school, k = seq_len(n) - 1, year = start + k, q = quality, rec = rec
  ) |> filter(year %in% YEARS) |>
    mutate(team = ifelse(pid == 9005 & k == 1, "Gonzaga", team),
           yr = c("Fr", "So", "Jr", "Sr")[k + 1],
           bpm = round(q + k * 0.8 + rnorm(n()), 2),
           GP = sample(25:34, n(), TRUE),
           mp = round(pmin(38, pmax(2, rnorm(n(), 22, 8))), 1),
           usg = 20 + q * 3, TS_per = 55 + q * 2,
           ht = paste0("6-", sample(0:11, n(), TRUE)),
           role = sample(ROLES, n(), TRUE))

  finals <- players |> transmute(pid, final_year = start + n - 1)
  draft <- seasons |> inner_join(finals, by = "pid") |> filter(year == final_year, year %in% DRAFTS) |>
    group_by(year) |> arrange(desc(bpm), .by_group = TRUE) |> mutate(pick = row_number()) |>
    filter(pick <= 40) |> ungroup() |> select(pid, year, pick, player_name, team)

  for (y in DRAFTS) {
    d <- draft |> filter(year == y) |>
      transmute(pick, player = player_name, college = team, team = "XXX", draft_year = y) |>
      bind_rows(tibble(pick = 41L, player = "Intl Guy", college = "", team = "XXX", draft_year = y)) |>
      mutate(college = recode(college, "Connecticut" = "UConn", "Pittsburgh" = "Pitt", "Miami FL" = "Miami (FL)", "N.C. State" = "NC State"),
             player = ifelse(player == "Gary Trent", "Gary Trent Jr.", player),
             player = ifelse(player == "Edrice Adebayo", "Bam Adebayo", player))
    write_csv(d, file.path(raw, sprintf("bref_draft_%d.csv", y)))
    # team ratings file WITH header, like the real one
    tm <- tibble(rank = seq_along(SCHOOLS), team = SCHOOLS, conf = "ACC", record = "20-10",
                 adjoe = round(rnorm(8, 110, 5), 1), adjde = round(rnorm(8, 95, 5), 1),
                 barthag = round(runif(8, 0.5, 0.99), 3), sos = round(rnorm(8, 5, 2), 1), WAB = round(rnorm(8, 0, 3), 1))
    write_csv(tm, file.path(raw, sprintf("teams_%d.csv", y)))
  }

  for (y in YEARS) {
    s <- seasons |> filter(year == y) |> left_join(draft |> select(pid, year, pick), by = c("pid", "year"))
    bt <- as.data.frame(matrix(0, nrow(s), length(BT_COLUMNS))); names(bt) <- BT_COLUMNS
    bt$player_name <- s$player_name; bt$team <- s$team; bt$conf <- "ACC"; bt$GP <- s$GP; bt$Min_per <- 60
    bt$ORtg <- 105; bt$usg <- s$usg; bt$eFG <- 50; bt$TS_per <- s$TS_per; bt$ORB_per <- 5; bt$DRB_per <- 15
    bt$AST_per <- 15; bt$TO_per <- 15; bt$FTM <- 100; bt$FTA <- 130; bt$FT_per <- 75
    bt$twoPM <- 120; bt$twoPA <- 250; bt$TPM <- 30; bt$TPA <- 90; bt$TP_per <- 33; bt$blk_per <- 2; bt$stl_per <- 2
    bt$ftr <- 30; bt$yr <- s$yr; bt$ht <- s$ht; bt$num <- 1; bt$porpag <- 2; bt$adjoe <- 110; bt$pfr <- 3
    bt$year <- y; bt$pid <- s$pid; bt$type <- "All"; bt$Rec_Rank <- s$rec; bt$ast_tov <- 1.2
    bt$rim_att <- 100; bt$rim_pct <- 0.6; bt$mid_pct <- 0.4; bt$dunksmade <- 5; bt$dunk_att <- 6
    bt$pick <- ifelse(is.na(s$pick), "", s$pick); bt$drtg <- 100; bt$adrtg <- 98; bt$dporpag <- 1; bt$stops <- 50
    bt$bpm <- s$bpm; bt$obpm <- s$bpm / 2; bt$dbpm <- s$bpm / 2; bt$gbpm <- s$bpm; bt$mp <- s$mp
    bt$pts <- 12; bt$ast <- 3; bt$treb <- 5; bt$stl <- 1; bt$blk <- 1; bt$role <- s$role
    write_csv(bt, file.path(raw, sprintf("barttorvik_%d.csv", y)), col_names = FALSE)
  }
  cat("fixture written to", raw, "\n")
}
