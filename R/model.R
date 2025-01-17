source("R/build_leagues.R")
source("R/load_season_odds.R")
source("R/get_monte_carlo_dist.R")
source("R/get_no_vig_probs.R")
source("R/get_points_prob.R")

for (league in leagues) {
    odds <- load_season_odds(league$id, league$name)
    probs <- get_no_vig_probs(odds)

    standings <- get_monte_carlo_dist(probs, n = 10)

    actual_points <- get_points_prob(probs)
    expected_points <- probs |>
        tidytable::mutate(
            exp = 3 * win + draw
        ) |>
        tidytable::group_by(team) |>
        tidytable::summarise(
            expected = sum(exp)
        ) |>
        tidytable::ungroup()

    points <- actual_points |>
        tidytable::left_join(expected_points)
    standings <- standings |>
        tidytable::left_join(points)

    file_name <- janitor::make_clean_names(league$name)
    write.csv(standings, paste0("data/", file_name, ".csv"))
    saveRDS(standings, paste0("data/", file_name, ".rds"))
}
