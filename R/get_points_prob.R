get_points_prob <- function(probs) {
    teams <- unique(probs$team)

    points <- tidytable::tidytable(
        team = character(0),
        actual_points_prob = numeric(0)
    )

    for (sel_team in teams) {
        team_probs <- probs |>
            tidytable::filter(
                team == sel_team
            )

        n <- nrow(team_probs)
        max_points <- 3 * n

        pd <- numeric(max_points + 1)
        pd[1] <- 1

        for (i in 1:n) {
            for (j in (3 * i + 1):1) {
                pd[j] <- (
                    (if (j > 3) pd[j - 3] * team_probs$win[i] else 0) +
                        (if (j > 1) pd[j - 1] * team_probs$draw[i] else 0) +
                        pd[j] * team_probs$loss[i]
                )
            }
        }

        actual_points <- sum(team_probs$result)
        team_points <- tidytable::tidytable(
            team = sel_team,
            actual_points_prob = pd[actual_points + 1]
        )
        points <- tidytable::bind_rows(list(points, team_points))
    }

    return (points)
}
