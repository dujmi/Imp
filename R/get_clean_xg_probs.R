get_clean_xg_probs <- function(matches, name) {
    home_probs <- matches |>
        tidytable::rename(
            team = home_team,
            win = forecast_win,
            draw = forecast_draw,
            loss = forecast_loss
        ) |>
        tidytable::mutate(
            result = tidytable::case_when(
                home_goals > away_goals ~ 3,
                home_goals == away_goals ~ 1,
                away_goals > home_goals ~ 0
            ),
            comp = name,
            season = 24
        )

    away_probs <- matches |>
        tidytable::rename(
            team = away_team,
            win = forecast_loss,
            draw = forecast_draw,
            loss = forecast_win
        ) |>
        tidytable::mutate(
            result = tidytable::case_when(
                away_goals > home_goals ~ 3,
                home_goals == away_goals ~ 1,
                home_goals > away_goals ~ 0
            ),
            comp = name,
            season = 24
        )

    probs <- tidytable::bind_rows(list(home_probs, away_probs)) |>
        tidytable::select(datetime, comp, season, team, win, draw, loss, result)

    return (probs)
}


