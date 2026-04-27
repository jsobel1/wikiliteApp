# history_flow_alluvial.R
# Faithful implementation of Viégas, Wattenberg & Dave (CHI 2004) Figure 2.
#
# Uses a proportional-deletion model as an approximation of full-text diffing:
#   - Additions → attributed entirely to the current editor
#   - Deletions → all editors' shares shrink proportionally

#' Estimate byte-level content ownership per editor at each revision
#'
#' Proportional-deletion model:
#' * When editor X adds content, their ownership grows by delta bytes.
#' * When any content is removed, all editors' shares shrink by the same ratio.
#'
#' @param hist Prepared history data frame (from [prepare_history()]).
#' @return Long data frame: `rev_index`, `ts_posix`, `author`, `author_label`,
#'   `bytes`.
#' @export
build_content_ownership <- function(hist) {
  label_map <- dplyr::distinct(hist, user, author_label)
  own       <- numeric(0)
  rows      <- vector("list", nrow(hist))

  for (i in seq_len(nrow(hist))) {
    size_i    <- hist$size[[i]]
    user_i    <- hist$user[[i]]
    size_prev <- if (i == 1L) 0L else hist$size[[i - 1L]]
    delta     <- size_i - size_prev

    if (i == 1L || length(own) == 0L) {
      own <- setNames(as.numeric(size_i), user_i)
    } else if (delta > 0L) {
      if (user_i %in% names(own)) own[[user_i]] <- own[[user_i]] + delta
      else own <- c(own, setNames(as.numeric(delta), user_i))
    } else if (delta < 0L && size_prev > 0L) {
      scale <- max(0, size_i / size_prev)
      own   <- own * scale
      own   <- own[own > 5]
    }

    tot <- sum(own)
    if (tot > 0 && abs(tot - size_i) > 1) own <- own * (size_i / tot)

    rows[[i]] <- tibble::tibble(
      rev_index = i,
      ts_posix  = hist$ts_posix[[i]],
      author    = names(own),
      bytes     = as.numeric(own)
    )
  }

  dplyr::bind_rows(rows) %>%
    dplyr::left_join(label_map, by = c("author" = "user")) %>%
    dplyr::mutate(author_label = dplyr::coalesce(author_label, author))
}

#' Subsample history to at most `max_n` evenly-spaced revisions
#'
#' @param hist  Prepared history data frame.
#' @param max_n Maximum number of revisions to keep.
#' @return Subsampled data frame with reindexed `rev_index`.
#' @export
subsample_history <- function(hist, max_n = 40) {
  if (nrow(hist) <= max_n) return(hist)
  idx <- unique(round(seq(1L, nrow(hist), length.out = max_n)))
  hist[idx, ] %>% dplyr::mutate(rev_index = dplyr::row_number())
}

#' Collapse minor editors into "Other" to keep plots readable
#'
#' @param own_df Output of [build_content_ownership()].
#' @param n_top  Number of top editors to keep individually.
#' @return Summarised data frame with `grp` column.
#' @export
collapse_minor_authors <- function(own_df, n_top = 8) {
  top <- own_df %>%
    dplyr::group_by(author) %>%
    dplyr::summarise(total = sum(bytes), .groups = "drop") %>%
    dplyr::slice_max(total, n = n_top, with_ties = FALSE) %>%
    dplyr::pull(author)

  own_df %>%
    dplyr::mutate(
      grp = dplyr::if_else(author %in% top, author_label, "Other"),
      grp = dplyr::if_else(grepl("^\\d|:", author) & !(author %in% top),
                           "Anonymous", grp)
    ) %>%
    dplyr::group_by(rev_index, ts_posix, grp) %>%
    dplyr::summarise(bytes = sum(bytes), .groups = "drop")
}

#' History flow alluvial diagram (closest R equivalent of CHI 2004, Fig. 2B)
#'
#' Requires the **ggalluvial** package.
#'
#' @param hist           Prepared history data frame.
#' @param max_revisions  Maximum revision snapshots to show (subsampled if needed).
#' @param spacing        `"revision"` (equal width) or `"date"` (proportional to time).
#' @param n_top          Top editors to colour individually; rest → `"Other"`.
#' @param interactive    `TRUE` → plotly widget; `FALSE` → static ggplot2.
#' @return A plotly widget or ggplot2 object.
#' @export
plot_history_flow_alluvial <- function(hist,
                                        max_revisions = 40,
                                        spacing       = c("revision", "date"),
                                        n_top         = 8,
                                        interactive   = FALSE) {
  spacing <- match.arg(spacing)
  h       <- subsample_history(hist, max_revisions)
  own_df  <- build_content_ownership(h)
  own_c   <- collapse_minor_authors(own_df, n_top)

  ts_map   <- dplyr::distinct(own_c, rev_index, ts_posix)
  all_grps <- unique(own_c$grp)
  all_revs <- unique(own_c$rev_index)

  full <- tidyr::expand_grid(rev_index = all_revs, grp = all_grps) %>%
    dplyr::left_join(own_c, by = c("rev_index", "grp")) %>%
    dplyr::left_join(ts_map, by = "rev_index") %>%
    dplyr::mutate(
      ts_posix = dplyr::coalesce(ts_posix.x, ts_posix.y),
      bytes    = tidyr::replace_na(bytes, 0)
    ) %>%
    dplyr::select(rev_index, ts_posix, grp, bytes)

  pal   <- author_palette(all_grps)
  x_col <- if (spacing == "date") "ts_posix" else "rev_index"
  x_lab <- if (spacing == "date") "Date"     else "Revision snapshot"

  p <- ggplot2::ggplot(full,
              ggplot2::aes(
                x        = .data[[x_col]],
                y        = bytes / 1000,
                stratum  = grp,
                alluvium = grp,
                fill     = grp
              )) +
    ggalluvial::geom_flow(
      stat      = "alluvium",
      aes.bind  = "alluvia",
      alpha     = 0.65,
      colour    = NA,
      decreasing = FALSE
    ) +
    ggalluvial::geom_stratum(
      alpha     = 0.90,
      width     = 0.38,
      colour    = "white",
      linewidth = 0.25
    ) +
    ggplot2::scale_fill_manual(values = pal, name = "Editor") +
    ggplot2::labs(
      title    = paste0("History Flow — ", unique(hist$art)[[1]]),
      subtitle = sprintf(
        "%d revision snapshots  |  spacing by %s  |  proportional-deletion model",
        length(all_revs), spacing
      ),
      x = x_lab, y = "Estimated content (kB)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position  = "right",
      panel.grid.minor = ggplot2::element_blank()
    )

  if (spacing == "date") {
    span_years <- as.numeric(
      difftime(max(full$ts_posix), min(full$ts_posix), units = "days")
    ) / 365
    brk <- if (span_years > 10) "2 years" else "1 year"
    p <- p + ggplot2::scale_x_datetime(
      labels = scales::date_format("%Y"),
      breaks = scales::date_breaks(brk)
    )
  }

  if (interactive) plotly::ggplotly(p, tooltip = c("x", "y", "fill")) else p
}

#' Side-by-side comparison: revision-spaced vs. date-spaced alluvial (static)
#'
#' Requires the **cowplot** package.
#'
#' @param hist          Prepared history data frame.
#' @param max_revisions Maximum revision snapshots.
#' @param n_top         Top editors to colour individually.
#' @return A cowplot combined plot.
#' @export
plot_alluvial_comparison <- function(hist, max_revisions = 40, n_top = 8) {
  if (!requireNamespace("cowplot", quietly = TRUE))
    stop("Package 'cowplot' is required for this function.")
  p1 <- plot_history_flow_alluvial(hist, max_revisions, "revision", n_top) +
    ggplot2::labs(title = "Revision-spaced") +
    ggplot2::theme(legend.position = "none")
  p2 <- plot_history_flow_alluvial(hist, max_revisions, "date", n_top) +
    ggplot2::labs(title = "Date-spaced")
  cowplot::plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 1))
}

#' Interactive Sankey diagram of content ownership across revision snapshots
#'
#' Each node is an (editor, revision) pair. Flows show how many bytes of each
#' editor's content survive to the next snapshot.
#'
#' @param hist          Prepared history data frame.
#' @param max_revisions Number of revision snapshots (keep ≤ 20 for clarity).
#' @param n_top         Top editors to colour; rest grouped as `"Other"`.
#' @return A plotly Sankey widget.
#' @export
plot_history_flow_sankey <- function(hist, max_revisions = 20, n_top = 8) {
  h     <- subsample_history(hist, max_revisions)
  own_df <- build_content_ownership(h)
  own_c  <- collapse_minor_authors(own_df, n_top) %>%
    dplyr::filter(bytes > 0)

  pal <- author_palette(unique(own_c$grp))

  nodes <- own_c %>%
    dplyr::distinct(rev_index, grp) %>%
    dplyr::arrange(rev_index, grp) %>%
    dplyr::mutate(
      node_id    = dplyr::row_number() - 1L,
      node_label = paste0("v", rev_index, " · ", grp),
      node_color = pal[grp] %||% "#CCCCCC"
    )

  src <- own_c %>% dplyr::rename(r_src = rev_index, b_src = bytes)
  tgt <- own_c %>% dplyr::rename(r_tgt = rev_index, b_tgt = bytes) %>%
    dplyr::mutate(r_src = r_tgt - 1L)

  links <- dplyr::inner_join(src, tgt, by = c("grp", "r_src")) %>%
    dplyr::filter(b_src > 0, b_tgt > 0) %>%
    dplyr::left_join(dplyr::select(nodes, rev_index, grp, node_id),
                     by = c("r_src" = "rev_index", "grp")) %>%
    dplyr::rename(source = node_id) %>%
    dplyr::left_join(dplyr::select(nodes, rev_index, grp, node_id),
                     by = c("r_tgt" = "rev_index", "grp")) %>%
    dplyr::rename(target = node_id) %>%
    dplyr::filter(!is.na(source), !is.na(target)) %>%
    dplyr::mutate(value = pmin(b_src, b_tgt) / 1000)

  plotly::plot_ly(
    type        = "sankey",
    arrangement = "snap",
    node = list(
      label     = nodes$node_label,
      color     = nodes$node_color,
      pad       = 10,
      thickness = 18,
      line      = list(color = "white", width = 0.5)
    ),
    link = list(
      source = links$source,
      target = links$target,
      value  = links$value,
      color  = "rgba(180,180,180,0.35)"
    )
  ) %>%
    plotly::layout(
      title = list(
        text = paste0("History Flow (Sankey) — ", unique(hist$art)[[1]],
                      "<br><sup>Estimated surviving content (kB) per editor</sup>"),
        font = list(size = 14)
      ),
      font = list(size = 9)
    )
}
