# fetch_data.R — wikilite wrappers with file-based caching and data preparation

#' Fetch and cache a full article revision history via wikilite
#'
#' @param article   Wikipedia article title (exact spelling).
#' @param date_an   Upper timestamp, e.g. `"2026-01-01T00:00:00Z"`.
#' @param lang      Language edition (default `"en"`).
#' @param cache_dir Directory for `.rds` cache files; `NULL` to disable caching.
#'
#' @return Raw data frame from `wikilite::get_article_full_history_table()`.
#' @export
fetch_history <- function(article,
                          date_an   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
                          lang      = "en",
                          cache_dir = "cache") {
  if (!is.null(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    slug  <- gsub("[^A-Za-z0-9_-]", "_", paste0(article, "_", lang))
    rpath <- file.path(cache_dir, paste0(slug, ".rds"))
    if (file.exists(rpath)) {
      message("  [cache] Loading '", article, "' from ", rpath)
      return(readRDS(rpath))
    }
  }

  message("  [wikilite] Fetching '", article, "' …")
  .has_lang <- "lang" %in% names(formals(wikilite::get_article_full_history_table))
  hist <- tryCatch(
    if (.has_lang)
      wikilite::get_article_full_history_table(article, date_an = date_an, lang = lang)
    else
      wikilite::get_article_full_history_table(article, date_an = date_an),
    error = function(e) stop("Failed to fetch '", article, "': ", conditionMessage(e))
  )

  if (!is.null(cache_dir)) {
    saveRDS(hist, rpath)
    message("  [cache] Saved to ", rpath)
  }
  hist
}

#' Prepare a raw wikilite history table for analysis
#'
#' Adds columns: `ts_posix`, `is_anon`, `is_rev`, `author_label`,
#' `cum_max_size`, `size_ratio`, `size_delta`.
#'
#' @param hist  Raw data frame from [fetch_history()].
#' @param n_top Number of top registered editors to label individually.
#'
#' @return Augmented data frame sorted oldest-first.
#' @export
prepare_history <- function(hist, n_top = 12) {
  if ("ts" %in% names(hist) && !"timestamp" %in% names(hist)) {
    hist$timestamp <- hist$ts
  }

  if (!"tags"    %in% names(hist)) hist$tags    <- ""
  if (!"comment" %in% names(hist)) hist$comment <- ""

  hist %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(
      ts_posix     = parse_ts(timestamp),
      rev_index    = dplyr::row_number(),
      is_anon      = is_anonymous(user),
      is_rev       = is_revert_edit(tags, comment),
      author_label = label_authors(user, is_anon, n_top = n_top),
      cum_max_size = cummax(size),
      size_ratio   = dplyr::if_else(
                       dplyr::lag(cum_max_size, default = dplyr::first(size)) > 0,
                       size / dplyr::lag(cum_max_size, default = dplyr::first(size)),
                       1
                     ),
      size_delta   = size - dplyr::lag(size, default = dplyr::first(size))
    )
}

#' Fetch and prepare histories for a vector of articles
#'
#' Emits per-article progress when run inside a Shiny session (via
#' \code{shiny::withProgress()} / \code{incProgress()} so end-users see a
#' progress bar with a percentage + per-article detail), and falls back to
#' \code{cli} progress on the console otherwise.
#'
#' @param articles  Character vector of Wikipedia article titles.
#' @param date_an   Upper timestamp.
#' @param lang      Language edition.
#' @param cache_dir Cache directory.
#'
#' @return Named list of prepared data frames (one per article).
#' @export
fetch_many <- function(articles,
                       date_an   = "2026-01-01T00:00:00Z",
                       lang      = "en",
                       cache_dir = "cache") {
  n <- length(articles)
  if (n == 0L) return(setNames(list(), character()))

  in_shiny <- requireNamespace("shiny", quietly = TRUE) &&
              isTRUE(tryCatch(shiny::isRunning(), error = function(e) FALSE))

  one <- function(a, i) {
    detail <- sprintf("[%d/%d] %s", i, n, a)
    if (in_shiny) {
      shiny::incProgress(1 / n, detail = detail)
    } else {
      message("  ", detail)
    }
    prepare_history(fetch_history(a, date_an, lang, cache_dir))
  }

  if (in_shiny) {
    shiny::withProgress(
      message = "Fetching Wikipedia histories",
      detail  = sprintf("0 / %d", n),
      value   = 0,
      {
        out <- setNames(
          lapply(seq_along(articles), function(i) one(articles[[i]], i)),
          articles
        )
      }
    )
  } else {
    out <- setNames(
      lapply(seq_along(articles), function(i) one(articles[[i]], i)),
      articles
    )
  }
  out
}
