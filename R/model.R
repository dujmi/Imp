source("R/build_leagues.R")
source("R/load_season_odds.R")
source("R/get_clean_xg_probs.R")
source("R/get_monte_carlo_dist.R")
source("R/get_no_vig_probs.R")
source("R/get_points_prob.R")
source("R/get_team_mappings.R")
source("R/get_standings_figure.R")

sources <- c("xG", "Betting odds")
file_name_sources <- c("xg", "bet")
choice <- menu(sources, title = "Model's data source")
if (choice == 0) {
    stop("No source selected. Exiting...")
}
message(paste0("You have selected '", sources[choice], "' as model's data source."))

repeat {
    input <- readline("Enter N for Monte Carlo simulation (2-10000, 0 means exit): ")
    simulations_n <- suppressWarnings(as.integer(input))

    if (simulations_n == 0) {
        stop("No N selected. Exiting...")
    } else if (!is.na(simulations_n) && simulations_n >= 2 && simulations_n <= 10000) {
        message(paste0("Model will run ", simulations_n, " simulations.")) 
        break
    } else {
        message("Invalid input! Please try again or type 0 to exit the program.")
    }
}

for (league in leagues) {
    if (sources[choice] == "xG") {
        xg <- worldfootballR::understat_league_match_results(league$understat, 2024) |>
            tidytable::arrange(desc(datetime))
        last_game <- paste0(xg$home_team[1], " - ", xg$away_team[1])

        probs <- get_clean_xg_probs(xg, league$name)
    } else if (sources[choice] == "Betting odds") {
        odds <- load_season_odds(league$fdck, league$name)
        last_game <- paste0(odds$home_team[1], " - ", odds$away_team[1])

        probs <- get_no_vig_probs(odds)
    }
    standings <- get_monte_carlo_dist(probs, n = simulations_n)

    actual_points <- get_points_prob(probs)
    expected_points <- probs |>
        tidytable::mutate(
            exp = 3 * win + draw
        ) |>
        tidytable::group_by(team) |>
        tidytable::summarise(
            expected_points = sum(exp),
            points = sum(result)
        ) |>
        tidytable::ungroup() |>
        tidytable::mutate(
            points_diff = points - expected_points
        )

    points <- actual_points |>
        tidytable::left_join(expected_points)
    standings <- standings |>
        tidytable::left_join(points)

    file_name <- janitor::make_clean_names(league$name)
    write.csv(standings, paste0("data/", file_name_sources[choice], "/", file_name, ".csv"))
    saveRDS(standings, paste0("data/", file_name_sources[choice], "/", file_name, ".rds"))

    teams <- length(unique(standings$team))
    standings <- standings |>
        tidytable::mutate_rowwise(
            cl = sum(tidytable::c_across(2:4)),
            rel = sum(tidytable::c_across((teams - 1):(teams + 1)))
        )

    if (league$figure == TRUE) {
        standings <- get_team_mappings(standings) |>
            tidytable::mutate(
                espn_logo = paste0("https://a.espncdn.com/combiner/i?img=/i/teamlogos/soccer/500/", espn_id, ".png&h=200&w=200")
            ) |>
            tidytable::select(espn_logo, team_short, "1", cl, rel, actual_points_prob, points_diff, expected_points) |>
            tidytable::arrange(desc(expected_points))

        figure <- get_standings_figure(standings, last_game, sources[choice], simulations_n)
        gtUtils::gt_save_crop(
            figure,
            file = paste0("figures/", file_name_sources[choice], "/", file_name, ".png"),
            bg = "#FFFFFF",
            whitespace = 40,
            zoom = 2,
            expand = 5
        )
    }
}
