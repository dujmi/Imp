load_season_odds <- function(id, name) {
    season <- 24

    odds <- tidytable::tidytable(
        tidytable::fread(
            sprintf(
                "https://www.football-data.co.uk/mmz4281/%d%d/%s.csv",
                season, season + 1, id
            ),
            showProgress = FALSE
        )
    ) |>
        tidytable::select(Date, Time, HomeTeam, AwayTeam, AvgCH, AvgCD, AvgCA, FTR) |>
        tidytable::drop_na() |>
        tidytable::mutate(
            Comp = name,
            Season = season,
            Datetime = lubridate::dmy_hm(paste(Date, Time))
        ) |>
        tidytable::arrange(desc(Datetime)) |>
        tidytable::select(-Date, -Time) |>
        janitor::clean_names()

    return (odds)
}
