get_no_vig_probs <- function(odds) {
    probs_grouped <- odds |>
        tidytable::mutate(
            h_vig = 1 / avg_ch,
            d_vig = 1 / avg_cd,
            a_vig = 1 / avg_ca,
            total_vig = h_vig + d_vig + a_vig,
            h_nv = h_vig / total_vig,
            d_nv = d_vig / total_vig,
            a_nv = a_vig / total_vig
        ) |>
        tidytable::select(datetime, comp, season, home_team, away_team, h_nv, d_nv, a_nv, ftr)

    home_probs <- probs_grouped |>
        tidytable::rename(
            team = home_team,
            win = h_nv,
            draw = d_nv,
            loss = a_nv
        ) |>
        tidytable::mutate(
            result = tidytable::case_when(
                ftr == "H" ~ 3,
                ftr == "D" ~ 1,
                ftr == "A" ~ 0
            )
        ) |>
        tidytable::select(-away_team, -ftr)
    away_probs <- probs_grouped |>
        tidytable::rename(
            team = away_team,
            win = a_nv,
            draw = d_nv,
            loss = h_nv
       ) |>
        tidytable::mutate(
            result = tidytable::case_when(
                ftr == "A" ~ 3,
                ftr == "D" ~ 1,
                ftr == "H" ~ 0
            )
        ) |>
        tidytable::select(-home_team, -ftr)
    probs <- tidytable::bind_rows(list(home_probs, away_probs))

    return (probs)
}
