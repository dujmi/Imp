get_team_mappings <- function(df) {
    team_names <- tidytable::fread("data/team_names.csv")
    team_codes <- tidytable::fread("data/team_codes.csv")

    mapped_df <- df |>
        tidytable::left_join(team_names, by = c("team" = "alternative")) |>
        tidytable::mutate(
            standard = ifelse(!is.na(name), name, team)
        ) |>
        tidytable::rename(team_name = standard) |>
        tidytable::select(-team, -name)

    mapped_df <- mapped_df |>
        tidytable::left_join(team_codes, by = "team_name")

    return (mapped_df)
}
