# authorship.R — editor contribution analysis

#' Overall authorship statistics for one article
#'
#' @param hist Prepared history data frame (from [prepare_history()]).
#' @return Named list of scalar metrics.
#' @export
authorship_stats <- function(hist) {
  n       <- nrow(hist)
  n_anon  <- sum(hist$is_anon)
  editors <- hist$user
  top1    <- names(sort(table(editors[!hist$is_anon]), dec = TRUE))[1]

  list(
    n_revisions      = n,
    n_unique_editors = length(unique(editors)),
    n_anonymous      = n_anon,
    pct_anonymous    = round(100 * n_anon / n, 1),
    n_registered     = n - n_anon,
    pct_registered   = round(100 * (n - n_anon) / n, 1),
    top_editor       = top1,
    top_editor_pct   = round(100 * sum(editors == top1) / n, 1),
    gini_editors     = gini_coefficient(table(editors))
  )
}

#' Gini coefficient for an edit-count distribution
#'
#' @param counts Named numeric vector or table of edit counts.
#' @return Single numeric value in \[0, 1\].
#' @export
gini_coefficient <- function(counts) {
  x <- sort(as.numeric(counts))
  n <- length(x)
  if (n == 0 || sum(x) == 0) return(NA_real_)
  round(2 * sum(x * seq_len(n)) / (n * sum(x)) - (n + 1) / n, 3)
}

#' Per-editor contribution table, ranked by edit count
#'
#' @param hist  Prepared history data frame.
#' @param n_top Maximum number of rows to return.
#' @return Data frame with one row per editor.
#' @export
editor_table <- function(hist, n_top = 20) {
  hist %>%
    dplyr::group_by(user, is_anon) %>%
    dplyr::summarise(
      n_edits         = dplyr::n(),
      n_reverts       = sum(is_rev, na.rm = TRUE),
      mean_size_delta = mean(size_delta, na.rm = TRUE),
      first_edit      = min(ts_posix),
      last_edit       = max(ts_posix),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      type        = dplyr::if_else(is_anon, "Anonymous", "Registered"),
      revert_rate = round(100 * n_reverts / n_edits, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(n_edits)) %>%
    dplyr::slice_head(n = n_top) %>%
    dplyr::select(user, type, n_edits, n_reverts, revert_rate,
                  mean_size_delta, first_edit, last_edit)
}

#' Estimate first-mover advantage: size profile across early milestones
#'
#' @param hist Prepared history data frame.
#' @return Tibble with columns revision, timestamp, size_kB, relative_to_v1.
#' @export
first_mover_profile <- function(hist) {
  milestones <- c(1, 5, 10, 25, 50, 100, 250, 500)
  milestones <- milestones[milestones <= nrow(hist)]

  purrr::map_dfr(milestones, function(m) {
    tibble::tibble(
      revision       = m,
      timestamp      = hist$ts_posix[m],
      size_kB        = hist$size[m] / 1000,
      relative_to_v1 = round(hist$size[m] / hist$size[1], 2)
    )
  })
}

#' Stacked bar: anonymous vs. registered edits per year
#'
#' @param hist Prepared history data frame.
#' @return A ggplot2 object.
#' @export
plot_anon_vs_registered <- function(hist) {
  df <- hist %>%
    dplyr::mutate(year = format(ts_posix, "%Y")) %>%
    dplyr::count(year, author_type = dplyr::if_else(is_anon, "Anonymous", "Registered"))

  ggplot2::ggplot(df, ggplot2::aes(x = year, y = n, fill = author_type)) +
    ggplot2::geom_col(position = "fill", colour = "white", linewidth = 0.2) +
    ggplot2::scale_fill_manual(
      values = c("Anonymous" = "#999999", "Registered" = "#377EB8"),
      name   = NULL
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      title    = paste0("Anonymous vs. Registered Edits — ", unique(hist$art)[1]),
      subtitle = sprintf("Overall: %.0f%% anonymous across %d revisions",
                         100 * mean(hist$is_anon), nrow(hist)),
      x = "Year", y = "Proportion of edits"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}

#' Bar chart of top editors by edit count
#'
#' @param hist  Prepared history data frame.
#' @param n_top Number of editors to show.
#' @return A ggplot2 object.
#' @export
plot_top_editors <- function(hist, n_top = 15) {
  df <- hist %>%
    dplyr::count(user, is_anon, sort = TRUE) %>%
    dplyr::slice_head(n = n_top) %>%
    dplyr::mutate(user = forcats::fct_reorder(user, n))

  ggplot2::ggplot(df, ggplot2::aes(
    x    = n, y = user,
    fill = dplyr::if_else(is_anon, "Anonymous", "Registered")
  )) +
    ggplot2::geom_col(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_manual(
      values = c("Anonymous" = "#999999", "Registered" = "#377EB8"),
      name   = NULL
    ) +
    ggplot2::labs(
      title = paste0("Top ", n_top, " Editors — ", unique(hist$art)[1]),
      x = "Number of revisions", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}

#' Cumulative edit count over time by author type
#'
#' @param hist Prepared history data frame.
#' @return A plotly widget.
#' @export
plot_cumulative_edits <- function(hist) {
  df <- hist %>%
    dplyr::arrange(ts_posix) %>%
    dplyr::group_by(author_type = dplyr::if_else(is_anon, "Anonymous", "Registered")) %>%
    dplyr::mutate(cum_edits = dplyr::row_number()) %>%
    dplyr::ungroup()

  p <- ggplot2::ggplot(df, ggplot2::aes(x = ts_posix, y = cum_edits, colour = author_type)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(
      values = c("Anonymous" = "#999999", "Registered" = "#377EB8"),
      name   = NULL
    ) +
    ggplot2::labs(
      title = paste0("Cumulative Edits Over Time — ", unique(hist$art)[1]),
      x = "Date", y = "Cumulative edits"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top")

  plotly::ggplotly(p)
}
