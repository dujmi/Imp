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
        tidytable::drop_na() |>
        tidytable::mutate(
            Comp = name,
            Season = season,
            Datetime = lubridate::dmy_hms(paste(Date, Time)),
        ) |>
        tidytable::arrange(desc(Datetime)) |>
        tidytable::select(-Date, -Time) |>
        janitor::clean_names()

    return (odds)
}
