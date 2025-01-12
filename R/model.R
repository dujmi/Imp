source("R/build_leagues.R")
source("R/load_season_odds.R")
source("R/get_standings_distribution.R")

for (league in leagues) {
    odds <- load_season_odds(league$id, league$name)

    odds <- odds |>
        tidytable::mutate(
            h_vig = 1 / avg_ch,
            d_vig = 1 / avg_cd,
            a_vig = 1 / avg_ca,
            total_vig = h_vig + d_vig + a_vig,
            h_nv = h_vig / total_vig,
            d_nv = d_vig / total_vig,
            a_nv = a_vig / total_vig
        ) |>
        tidytable::select(datetime, comp, season, home_team, away_team, h_nv, d_nv, a_nv)
}
