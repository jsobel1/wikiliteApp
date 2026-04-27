# stability.R — content stability analysis

#' Summary stability metrics for one article
#'
#' @param hist Prepared history data frame (from [prepare_history()]).
#' @return Named list of scalar metrics.
#' @export
stability_stats <- function(hist) {
  deltas    <- hist$size_delta
  shrinkage <- deltas[deltas < 0]
  growth    <- deltas[deltas > 0]

  list(
    n_revisions         = nrow(hist),
    final_size_kB       = round(tail(hist$size, 1) / 1000, 1),
    peak_size_kB        = round(max(hist$size)  / 1000, 1),
    size_growth_pct     = round(100 * (tail(hist$size, 1) - hist$size[1]) / hist$size[1], 1),
    pct_edits_shrink    = round(100 * mean(deltas < 0,  na.rm = TRUE), 1),
    pct_edits_grow      = round(100 * mean(deltas > 0,  na.rm = TRUE), 1),
    pct_edits_none      = round(100 * mean(deltas == 0, na.rm = TRUE), 1),
    mean_growth_bytes   = round(mean(growth,    na.rm = TRUE), 0),
    mean_shrink_bytes   = round(mean(shrinkage, na.rm = TRUE), 0),
    size_cv             = round(stats::sd(hist$size, na.rm = TRUE) /
                                  mean(hist$size, na.rm = TRUE), 3),
    pct_substantive_cuts = round(100 * mean(deltas < -50, na.rm = TRUE), 1)
  )
}

#' Rolling mean of article size (smoothed size trajectory)
#'
#' @param hist   Prepared history data frame.
#' @param window Window size in revisions.
#' @return Data frame with additional column `rolling_size`.
#' @export
rolling_mean_size <- function(hist, window = 20) {
  hist %>%
    dplyr::arrange(ts_posix) %>%
    dplyr::mutate(
      rolling_size = as.numeric(
        zoo::rollmean(size, k = window, fill = NA, align = "center")
      )
    )
}

#' Article size over time with smoothed trend
#'
#' @param hist        Prepared history data frame.
#' @param interactive `TRUE` → plotly widget; `FALSE` → ggplot2 object.
#' @return A plotly or ggplot2 object.
#' @export
plot_size_timeline <- function(hist, interactive = TRUE) {
  p <- ggplot2::ggplot(hist, ggplot2::aes(x = ts_posix, y = size / 1000)) +
    ggplot2::geom_line(colour = "steelblue", linewidth = 0.4, alpha = 0.7) +
    ggplot2::geom_smooth(method = "loess", span = 0.3,
                         colour = "darkblue", linewidth = 1, se = FALSE) +
    ggplot2::geom_point(
      data    = dplyr::filter(hist, size_delta < -50 * 10),
      mapping = ggplot2::aes(x = ts_posix, y = size / 1000),
      colour  = "firebrick", size = 1.5, shape = 19, alpha = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      title    = paste0("Article Size Over Time — ", unique(hist$art)[1]),
      subtitle = "Blue line = LOESS trend, red dots = large cuts (> 500 bytes)",
      x = "Date", y = "Article size (kB)"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  if (interactive) plotly::ggplotly(p) else p
}

#' Distribution of edit size deltas (growth vs. shrinkage)
#'
#' @param hist Prepared history data frame.
#' @return A ggplot2 object.
#' @export
plot_delta_distribution <- function(hist) {
  df <- dplyr::filter(hist, abs(size_delta) > 0, abs(size_delta) < 5000)
  ggplot2::ggplot(df, ggplot2::aes(
    x    = size_delta / 1000,
    fill = dplyr::if_else(size_delta > 0, "Growth", "Shrinkage")
  )) +
    ggplot2::geom_histogram(binwidth = 0.1, colour = "white", linewidth = 0.1) +
    ggplot2::geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
    ggplot2::scale_fill_manual(
      values = c("Growth" = "#4DAF4A", "Shrinkage" = "#E41A1C"),
      name   = NULL
    ) +
    ggplot2::labs(
      title    = paste0("Edit Size Distribution — ", unique(hist$art)[1]),
      subtitle = sprintf(
        "%.0f%% of edits add content, %.0f%% remove content",
        100 * mean(hist$size_delta > 0, na.rm = TRUE),
        100 * mean(hist$size_delta < 0, na.rm = TRUE)
      ),
      x = "Size change per edit (kB)", y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}

#' Average version size vs. version number across multiple articles
#'
#' Replicates Fig. 8 of Viégas et al. (CHI 2004).
#'
#' @param many_hist Named list of prepared history data frames.
#' @return A ggplot2 object.
#' @export
plot_size_vs_revision <- function(many_hist) {
  df <- purrr::imap_dfr(many_hist, function(hist, name) {
    dplyr::mutate(hist, article = name, rev_no = rev_index)
  })

  ggplot2::ggplot(df, ggplot2::aes(x = rev_no, y = size / 1000, colour = article)) +
    ggplot2::geom_smooth(method = "loess", span = 0.4, se = FALSE, linewidth = 0.8) +
    ggplot2::labs(
      title    = "Average Article Size vs. Revision Number",
      subtitle = "Replicates Fig. 8 of Viégas et al. (CHI 2004)",
      x = "Revision number", y = "Article size (kB)", colour = "Article"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Heatmap of editing intensity by hour-of-day x day-of-week
#'
#' @param hist Prepared history data frame.
#' @return A ggplot2 object.
#' @export
plot_editing_rhythm <- function(hist) {
  df <- hist %>%
    dplyr::mutate(
      hour = as.integer(format(ts_posix, "%H")),
      wday = weekdays(ts_posix, abbreviate = TRUE),
      wday = factor(wday, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
    ) %>%
    dplyr::count(hour, wday)

  ggplot2::ggplot(df, ggplot2::aes(x = hour, y = wday, fill = n)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Edits") +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, 3),
                                labels = sprintf("%02d:00", seq(0, 23, 3))) +
    ggplot2::labs(
      title    = paste0("Editing Rhythm (UTC) — ", unique(hist$art)[1]),
      subtitle = "Hour of day x Day of week",
      x = "Hour (UTC)", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}
