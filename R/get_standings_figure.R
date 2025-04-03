get_standings_figure <- function(standings, last_game, model, n) {
    if (model == "xG") {
        title <- "Simulacija očekivanih golova"
        source_note <- paste0("understat.com | @mislavdujak | Posljednja utakmica: ", last_game)
        footnote_explanation <- paste0("Stupci 'Prvi', 'Liga prvaka' i 'Relegacija' rezultat su Monte Carlo simulacije (n = ", n, ") dosadašnjeg tijeka sezone s obzirom na ostvarene očekivane golove")
    } else if (model == "Betting odds") {
        title <- "Procjene kladionica"
        source_note <- paste0("football-data.co.uk | @mislavdujak | Posljednja utakmica: ", last_game)
        footnote_explanation <- paste0("Stupci 'Prvi', 'Liga prvaka' i 'Relegacija' rezultat su Monte Carlo simulacije (n = ", n, ") dosadašnjeg tijeka sezone s obzirom na kladioničarske kvote")
    }

    figure <- gt::gt(standings) |>
        gt::tab_header(title) |>
        gt::tab_source_note(source_note) |>
        gt::cols_label(
            espn_logo = "",
            team_short = "",
            "1" = "Prvi",
            cl = "Liga prvaka",
            rel = "Relegacija",
            actual_points_prob = "Očekivanost",
            points_diff = "Razlika",
            expected_points = "xP"
        ) |>
        gt::tab_footnote(
            footnote = footnote_explanation,
            locations = gt::cells_column_labels(columns = "1")
        ) |>
        gt::tab_footnote(
            footnote = "Vjerojatnost skupljanja osvojenih bodova",
            locations = gt::cells_column_labels(columns = actual_points_prob)
        ) |>
        gt::tab_footnote(
            footnote = "Razlika između osvojenih bodova i očekivanih bodova",
            locations = gt::cells_column_labels(columns = points_diff)
        ) |>
        gt::cols_align(
            align = "center",
            columns = c("1", cl, rel, actual_points_prob, points_diff, expected_points)
        ) |>
        gt::cols_width(
            espn_logo ~ px(75),
            team_short ~ px(175),
            "1" ~ px(100),
            cl ~ px(100),
            rel ~ px(100),
            expected_points ~ px(150)
        ) |>
        gt::fmt_percent(
            columns = c("1", cl, rel, actual_points_prob),
            decimals = 0
        ) |>
        gt::fmt_number(
            columns = c(points_diff, expected_points),
            decimals = 1
        ) |>
        gtExtras::gt_img_rows(columns = espn_logo, height = 50) |>
        gtExtras::gt_hulk_col_numeric("1", trim = TRUE) |>
        gtExtras::gt_hulk_col_numeric(cl, trim = TRUE) |>
        gtExtras::gt_hulk_col_numeric(rel, trim = TRUE, reverse = TRUE) |>
        gtExtras::gt_hulk_col_numeric(expected_points, trim = TRUE) |>
        gtExtras::gt_theme_nytimes() |>
        gt::opt_horizontal_padding(3) |>
        gt::tab_options(
            table.font.color = "#111213",
            table.font.color.light = "#FFFFFF",
            row_group.border.bottom.width = 1,
            row_group.border.top.width = 1,
            source_notes.padding = 0,
            source_notes.border.bottom.width = 0,
            heading.padding = 20,
            column_labels.padding = 8,
            table_body.hlines.color = "#DBDCDF",
            row_group.border.top.color = "#DBDCDF",
            row_group.border.bottom.color = "#DBDCDF",
            footnotes.padding = 24,
            footnotes.multiline = FALSE,
            footnotes.padding.horizontal = 12,
            table_body.border.bottom.width = 0,
            table.border.bottom.width = 0
        ) |>
        gt::tab_style(
            style = gt::cell_text(
                color = "#494C50",
                font = gt::google_font("Epilogue"),
                transform = "uppercase",
                size = gt::px(18),
                align = "center"
            ),
            locations = list(
                gt::cells_column_labels(), gt::cells_column_spanners()
            )
        ) |>
        gt::tab_style(
            style = gt::cell_text(
                color = "#111213",
                font = gt::google_font("Epilogue"),
                size = gt::px(28),
                weight = 800,
                align = "center"
            ),
            locations = list(
                gt::cells_title(groups = "title")
            )
        ) |>
        gt::tab_style(
            style = gt::cell_text(
                color = "#111213",
                font = gt::google_font("Epilogue"),
                v_align = "middle"
            ),
            locations = list(
                gt::cells_body()
            )
        ) |>
        gt::tab_style(
            style = gt::cell_text(
                color = "#111213",
                font = gt::google_font("Epilogue"),
                size = gt::px(14),
                weight = 400
            ),
            locations = list(
                gt::cells_footnotes(), gt::cells_source_notes()
            )
        )

    return(figure)
}
