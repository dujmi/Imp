get_monte_carlo_dist <- function(probs, n = 10000) {
    results <- tidytable::tidytable(
        team = character(0),
        position = integer(0)
    )

    for (i in seq_len(n)) {
        outcomes <- probs |>
            tidytable::mutate(
                result = tidytable::pmap_chr(
                    list(win = win, draw = draw, loss = loss),
                    function(win, draw, loss) {
                        sample(
                            c("win", "draw", "loss"), 
                            size = 1, 
                            prob = c(win, draw, loss)
                        )
                    }
                )
            )

        team_results <- outcomes |>
            tidytable::group_by(team) |>
            tidytable::summarise(
                wins = sum(result == "win"),
                draws = sum(result == "draw"),
                rtie = runif(1)
            ) |>
            tidytable::ungroup() |>
            tidytable::mutate(
                points = 3 * wins + 1 * draws,
            ) |>
            tidytable::arrange(desc(points), rtie) |>
            tidytable::mutate(
                position = tidytable::row_number()
            )

        results <- tidytable::bind_rows(list(results, team_results))
    }

    standings <- results |>
        tidytable::group_by(team, position) |>
        tidytable::summarise(probability = n() / n) |>
        tidytable::ungroup() |>
        tidytable::arrange(position, desc(probability)) |>
        tidytable::pivot_wider(
            names_from = position,
            values_from = probability
        ) |>
        tidytable::mutate(across(where(is.numeric), ~ tidytable::replace_na(.x, 0))) |>
        tidytable::arrange(desc(1))

    return (standings)
}
