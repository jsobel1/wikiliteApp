# vandalism.R — mass-deletion detection and survival analysis

#' Detect mass-deletion vandalism events in a revision history
#'
#' A "mass deletion" is defined as a revision where the article size drops to
#' ≤ `threshold` of the running maximum (i.e. at least 90% of content is
#' removed in a single edit), following Viégas et al. (CHI 2004), Table 1.
#'
#' @param hist      Prepared history data frame (from [prepare_history()]).
#' @param threshold Size ratio below which a revision is flagged (default 0.10).
#' @return The same data frame with a logical column `is_vandalism` added.
#' @export
detect_vandalism <- function(hist, threshold = 0.10) {
  hist %>%
    dplyr::mutate(
      is_vandalism = size_ratio < threshold &
        dplyr::lag(cum_max_size, default = dplyr::first(cum_max_size)) > 500
    )
}

#' Compute repair (survival) time for each detected vandalism event
#'
#' For every mass-deletion revision, scans forward to find the first revision
#' that restores the article to ≥ `restore_frac` of the pre-deletion maximum.
#'
#' @param hist         Prepared + vandalism-flagged data frame.
#' @param restore_frac Fraction of pre-deletion size needed to call a repair.
#' @return Data frame with one row per vandalism event.
#' @export
compute_survival <- function(hist, restore_frac = 0.50) {
  van_idx <- which(hist$is_vandalism)
  if (length(van_idx) == 0) {
    message("No mass deletions found.")
    return(tibble::tibble())
  }

  purrr::map_dfr(van_idx, function(i) {
    size_before <- hist$cum_max_size[i]
    needed      <- size_before * restore_frac
    future      <- hist[(i + 1):nrow(hist), ]
    repair_j    <- which(future$size >= needed)[1]

    tibble::tibble(
      revid        = hist$revid[i],
      timestamp    = hist$ts_posix[i],
      user         = hist$user[i],
      is_anon      = hist$is_anon[i],
      size_before  = size_before,
      size_after   = hist$size[i],
      size_ratio   = hist$size_ratio[i],
      repair_revid = if (!is.na(repair_j)) future$revid[repair_j]    else NA_integer_,
      repair_ts    = if (!is.na(repair_j)) future$ts_posix[repair_j] else as.POSIXct(NA),
      survival_min = if (!is.na(repair_j))
                       minutes_between(hist$ts_posix[i], future$ts_posix[repair_j])
                     else NA_real_,
      was_repaired = !is.na(repair_j)
    )
  })
}

#' Print survival-time statistics matching Table 1 of Viégas et al.
#'
#' @param survival_df Output of [compute_survival()].
#' @return Invisibly returns `survival_df`.
#' @export
summarise_survival <- function(survival_df) {
  if (nrow(survival_df) == 0) return(invisible(NULL))
  repaired <- dplyr::filter(survival_df, was_repaired)
  cat(sprintf("Mass deletions detected : %d\n", nrow(survival_df)))
  cat(sprintf("Repaired (within history): %d (%.0f%%)\n",
              nrow(repaired), 100 * nrow(repaired) / nrow(survival_df)))
  if (nrow(repaired) > 0) {
    cat(sprintf("Survival time — mean   : %.1f min\n",
                mean(repaired$survival_min,   na.rm = TRUE)))
    cat(sprintf("Survival time — median : %.1f min\n",
                stats::median(repaired$survival_min, na.rm = TRUE)))
  }
  invisible(survival_df)
}

#' Timeline of article size with vandalism events highlighted
#'
#' @param hist        History data frame (must contain `is_vandalism` column).
#' @param survival_df Output of [compute_survival()].
#' @param interactive `TRUE` → plotly widget; `FALSE` → ggplot2 object.
#' @return A plotly or ggplot2 object.
#' @export
plot_vandalism_timeline <- function(hist, survival_df, interactive = TRUE) {
  p <- ggplot2::ggplot(hist, ggplot2::aes(x = ts_posix, y = size / 1000)) +
    ggplot2::geom_line(colour = "steelblue", linewidth = 0.5) +
    ggplot2::geom_point(
      data    = dplyr::filter(hist, is_vandalism),
      mapping = ggplot2::aes(x = ts_posix, y = size / 1000),
      colour  = "red", size = 2.5, shape = 19,
      inherit.aes = FALSE
    ) +
    (if (nrow(survival_df) > 0 && any(survival_df$was_repaired))
      ggplot2::geom_segment(
        data    = dplyr::filter(survival_df, was_repaired),
        mapping = ggplot2::aes(
          x = timestamp, xend = repair_ts,
          y = size_after / 1000, yend = size_before / 1000
        ),
        colour = "orange", linewidth = 0.8, linetype = "dashed",
        inherit.aes = FALSE
      )
    else NULL) +
    ggplot2::labs(
      title    = paste0("Vandalism Timeline — ", unique(hist$art)[1]),
      subtitle = sprintf(
        "%d mass deletions detected  (red = vandalism, orange = repair path)",
        nrow(survival_df)
      ),
      x = "Date", y = "Article size (kB)"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  if (interactive) plotly::ggplotly(p) else p
}

#' Survival-time distribution histogram
#'
#' @param survival_df Output of [compute_survival()].
#' @return A ggplot2 object, or `NULL` invisibly if nothing to plot.
#' @export
plot_survival_dist <- function(survival_df) {
  repaired <- dplyr::filter(survival_df, was_repaired, survival_min < 60 * 24 * 7)
  if (nrow(repaired) == 0) {
    message("No repaired vandalism events to plot.")
    return(invisible(NULL))
  }
  med_val <- stats::median(repaired$survival_min, na.rm = TRUE)
  ggplot2::ggplot(repaired, ggplot2::aes(x = survival_min)) +
    ggplot2::geom_histogram(binwidth = 5, fill = "steelblue", colour = "white") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = med_val),
                        colour = "red", linetype = "dashed", linewidth = 0.8) +
    ggplot2::annotate("text",
                      x = med_val + 2, y = Inf, vjust = 2, hjust = 0,
                      size = 3.5, colour = "red",
                      label = sprintf("Median = %.1f min", med_val)) +
    ggplot2::labs(
      title    = "Vandalism Repair Time Distribution",
      subtitle = "Revisions within 1 week shown; dashed line = median",
      x = "Minutes until repair", y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}
