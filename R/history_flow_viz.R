# history_flow_viz.R
# Approximation of Viégas, Wattenberg & Dave (CHI 2004) history flow
# visualization using article size as a proxy for content extent.

#' History flow plot for one article
#'
#' Produces a stacked area chart where each revision is coloured by the editor
#' who made it, with revert events overlaid as red downward triangles.
#'
#' @param hist        Prepared history data frame (from [prepare_history()]).
#' @param spacing     `"revision"` — equal width per revision;
#'                    `"date"` — x-axis proportional to real time.
#' @param n_top       Number of top editors to colour individually.
#' @param interactive `TRUE` → plotly widget; `FALSE` → ggplot2 object.
#' @return A plotly widget or ggplot2 object.
#' @export
plot_history_flow <- function(hist,
                               spacing     = c("revision", "date"),
                               n_top       = 12,
                               interactive = TRUE) {
  spacing   <- match.arg(spacing)
  art_title <- unique(hist$art)[1]
  pal       <- author_palette(unique(hist$author_label))
  reverts   <- dplyr::filter(hist, is_rev)

  hist <- hist %>%
    dplyr::mutate(x_val = if (spacing == "date") ts_posix else as.numeric(rev_index))

  p <- ggplot2::ggplot(hist, ggplot2::aes(x = x_val, y = size / 1000)) +
    ggplot2::geom_area(ggplot2::aes(fill = author_label),
                       alpha = 0.75, position = "identity") +
    ggplot2::geom_step(colour = "white", linewidth = 0.15, alpha = 0.6) +
    ggplot2::geom_point(
      data    = reverts,
      mapping = ggplot2::aes(x = x_val, y = size / 1000),
      shape = 25, size = 1.8, colour = "red", fill = "red", alpha = 0.9,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = pal, name = "Editor") +
    ggplot2::labs(
      title    = paste0("History Flow — ", art_title),
      subtitle = sprintf("Spacing by %s  |  %d revisions  |  %d editors",
                         spacing, nrow(hist), length(unique(hist$user))),
      y = "Article size (kB)",
      x = if (spacing == "date") "Date" else "Revision"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "right",
                   legend.key.size  = ggplot2::unit(0.55, "cm"),
                   panel.grid.minor = ggplot2::element_blank())

  if (spacing == "date") {
    p <- p + ggplot2::scale_x_datetime(
      labels = scales::date_format("%b %Y"),
      breaks = scales::date_breaks("1 year")
    )
  }

  if (interactive) {
    plotly::ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      plotly::layout(legend = list(orientation = "v", x = 1.02))
  } else {
    p
  }
}

#' Two-panel comparison: revision-spaced vs. date-spaced history flow
#'
#' Requires the **cowplot** package (`install.packages("cowplot")`).
#'
#' @param hist Prepared history data frame.
#' @return A cowplot combined plot (static only).
#' @export
plot_history_flow_comparison <- function(hist) {
  if (!requireNamespace("cowplot", quietly = TRUE))
    stop("Package 'cowplot' is required for this function.")
  p1 <- plot_history_flow(hist, spacing = "revision", interactive = FALSE) +
    ggplot2::labs(title = "Revision-spaced") +
    ggplot2::theme(legend.position = "none")
  p2 <- plot_history_flow(hist, spacing = "date", interactive = FALSE) +
    ggplot2::labs(title = "Date-spaced")
  cowplot::plot_grid(
    p1, p2, ncol = 1, align = "v",
    labels    = paste0("History Flow — ", unique(hist$art)[1]),
    label_size = 13, label_x = 0, hjust = 0
  )
}

#' Monthly edit activity heatmap per editor (top-N registered editors)
#'
#' @param hist  Prepared history data frame.
#' @param n_top Number of top editors to show.
#' @return A ggplot2 object.
#' @export
plot_editor_heatmap <- function(hist, n_top = 15) {
  top_editors <- hist %>%
    dplyr::filter(!is_anon) %>%
    dplyr::count(user, sort = TRUE) %>%
    dplyr::slice_head(n = n_top) %>%
    dplyr::pull(user)

  heat_df <- hist %>%
    dplyr::filter(user %in% top_editors) %>%
    dplyr::mutate(month = format(ts_posix, "%Y-%m")) %>%
    dplyr::count(user, month)

  ggplot2::ggplot(heat_df, ggplot2::aes(x = month, y = user, fill = n)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Edits") +
    ggplot2::labs(
      title    = paste0("Monthly Edit Activity — top ", n_top, " editors"),
      subtitle = unique(hist$art)[1],
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
      panel.grid  = ggplot2::element_blank()
    )
}
