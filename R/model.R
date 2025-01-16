source("R/build_leagues.R")
source("R/load_season_odds.R")
source("R/get_monte_carlo_dist.R")
source("R/get_no_vig_probs.R")

for (league in leagues) {
    odds <- load_season_odds(league$id, league$name)
    probs <- get_no_vig_probs(odds)

    standings <- get_monte_carlo_dist(probs, n = 10)

    file_name <- janitor::make_clean_names(league$name)
    write.csv(standings, paste0("data/", file_name, ".csv"))
    saveRDS(standings, paste0("data/", file_name, ".rds"))
}
