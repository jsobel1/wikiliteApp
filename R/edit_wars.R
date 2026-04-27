# edit_wars.R — edit-war detection and visualisation

#' Detect edit-war episodes using revert tags and alternating-user heuristic
#'
#' An episode starts when ≥ `min_reverts` consecutive revert-type edits occur
#' within a window of `window_size` revisions, with at least two distinct
#' editors involved.  Overlapping windows are merged into single episodes.
#'
#' @param hist        Prepared history data frame (from [prepare_history()]).
#' @param window_size Rolling window (in revisions) to scan for revert bursts.
#' @param min_reverts Minimum revert-tagged edits within the window.
#' @return Data frame of detected episodes.
#' @export
detect_edit_wars <- function(hist, window_size = 20, min_reverts = 3) {
  n <- nrow(hist)
  if (n < window_size) return(tibble::tibble())

  episodes <- list()
  for (i in seq_len(n - window_size + 1)) {
    w     <- hist[i:(i + window_size - 1), ]
    n_rev <- sum(w$is_rev, na.rm = TRUE)
    if (n_rev < min_reverts) next

    editors <- unique(w$user[w$is_rev])
    if (length(editors) < 2) next

    episodes[[length(episodes) + 1]] <- tibble::tibble(
      start_idx  = i,
      end_idx    = i + window_size - 1,
      start_ts   = w$ts_posix[1],
      end_ts     = w$ts_posix[nrow(w)],
      n_reverts  = n_rev,
      n_editors  = length(editors),
      editors    = paste(editors, collapse = " / "),
      duration_h = minutes_between(w$ts_posix[1], w$ts_posix[nrow(w)]) / 60
    )
  }

  if (length(episodes) == 0) return(tibble::tibble())

  eps_df  <- dplyr::bind_rows(episodes) %>% dplyr::arrange(start_idx)
  merged  <- list()
  current <- eps_df[1, ]
  for (j in seq_len(nrow(eps_df))[-1]) {
    r <- eps_df[j, ]
    if (r$start_idx <= current$end_idx) {
      current$end_idx   <- max(current$end_idx,  r$end_idx)
      current$end_ts    <- max(current$end_ts,   r$end_ts)
      current$n_reverts <- max(current$n_reverts, r$n_reverts)
      current$n_editors <- max(current$n_editors, r$n_editors)
    } else {
      merged[[length(merged) + 1]] <- current
      current <- r
    }
  }
  merged[[length(merged) + 1]] <- current
  dplyr::bind_rows(merged)
}

#' Detect size oscillation (fallback when tags are absent)
#'
#' Oscillation = the article size alternates direction at least `min_osc`
#' times within a window of `window_size` revisions.
#'
#' @param hist        Prepared history data frame.
#' @param window_size Number of revisions per window.
#' @param min_osc     Minimum direction changes in the window.
#' @return Data frame of detected oscillation periods.
#' @export
detect_size_oscillation <- function(hist, window_size = 15, min_osc = 5) {
  n   <- nrow(hist)
  eps <- list()

  for (i in seq_len(n - window_size + 1)) {
    w      <- hist[i:(i + window_size - 1), ]
    dirs   <- sign(diff(w$size))
    dirs   <- dirs[dirs != 0]
    n_flip <- sum(dirs[-1] != dirs[-length(dirs)], na.rm = TRUE)
    if (n_flip < min_osc) next

    eps[[length(eps) + 1]] <- tibble::tibble(
      start_idx  = i,
      end_idx    = i + window_size - 1,
      start_ts   = w$ts_posix[1],
      end_ts     = w$ts_posix[nrow(w)],
      n_flips    = n_flip,
      size_range = max(w$size) - min(w$size)
    )
  }
  if (length(eps) == 0) return(tibble::tibble())
  dplyr::bind_rows(eps) %>% dplyr::distinct(start_idx, .keep_all = TRUE)
}

#' Annotated size timeline highlighting edit-war periods
#'
#' @param hist        Prepared history data frame.
#' @param wars_df     Output of [detect_edit_wars()].
#' @param interactive `TRUE` → plotly widget; `FALSE` → ggplot2 object.
#' @return A plotly or ggplot2 object.
#' @export
plot_edit_wars <- function(hist, wars_df, interactive = TRUE) {
  p <- ggplot2::ggplot(hist, ggplot2::aes(x = ts_posix, y = size / 1000)) +
    ggplot2::geom_line(colour = "steelblue", linewidth = 0.5) +
    ggplot2::geom_point(
      data    = dplyr::filter(hist, is_rev),
      mapping = ggplot2::aes(x = ts_posix, y = size / 1000),
      colour  = "orange", size = 1.5, shape = 1,
      inherit.aes = FALSE
    )

  if (nrow(wars_df) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data    = wars_df,
        mapping = ggplot2::aes(xmin = start_ts, xmax = end_ts,
                               ymin = -Inf, ymax = Inf),
        fill = "firebrick", alpha = 0.12, inherit.aes = FALSE
      ) +
      ggplot2::geom_text(
        data    = wars_df,
        mapping = ggplot2::aes(
          x     = start_ts + (end_ts - start_ts) / 2,
          y     = Inf,
          label = paste0(n_reverts, " reverts")
        ),
        vjust = 1.6, size = 2.8, colour = "firebrick", inherit.aes = FALSE
      )
  }

  p <- p +
    ggplot2::labs(
      title    = paste0("Edit Wars — ", unique(hist$art)[1]),
      subtitle = sprintf(
        "%d edit-war episode(s) detected  (shaded = episode, ○ = revert)",
        nrow(wars_df)
      ),
      x = "Date", y = "Article size (kB)"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  if (interactive) plotly::ggplotly(p) else p
}

#' Detailed zoom into a single edit-war episode
#'
#' @param hist    Prepared history data frame.
#' @param war_row Single-row tibble from [detect_edit_wars()].
#' @return A ggplot2 object.
#' @export
plot_war_zoom <- function(hist, war_row) {
  window_hist <- hist[war_row$start_idx:war_row$end_idx, ]
  ggplot2::ggplot(window_hist, ggplot2::aes(x = rev_index, y = size / 1000, colour = user)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_point(
      data    = dplyr::filter(window_hist, is_rev),
      mapping = ggplot2::aes(x = rev_index, y = size / 1000),
      shape = 25, size = 3, colour = "red", fill = "red",
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      title    = paste0("Edit-War Zoom — ", unique(hist$art)[1]),
      subtitle = format(war_row$start_ts, "%Y-%m-%d"),
      x = "Revision index", y = "Article size (kB)", colour = "Editor"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}
