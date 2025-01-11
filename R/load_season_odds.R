load_season_odds <- function(id, name) {
    season <- 24

    odds <- tidytable::tidytable(
        readr::read_csv(
            sprintf(
                "https://www.football-data.co.uk/mmz4281/%d%d/%s.csv",
                season, season + 1, id
            ),
            lazy = TRUE
        )
    ) |>
        tidytable::select(Date, Time, HomeTeam, AwayTeam, AvgCH, AvgCD, AvgCA) |>
        tidytable::mutate(
            comp = name,
            season = season
        ) |>
        janitor::clean_names()

    return (odds)
}
