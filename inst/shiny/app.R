# =============================================================================
# Wikipedia History Flow — interactive Shiny app
#
# Replicates the Microsoft Wikipedia History Flow Tool (Viégas et al., CHI 2004)
#
# Top-level navigation:
#   Single Article   — one article, multi-tab analysis (incl. Revision Inspector)
#   Corpus Analysis  — one or more named/categorised article lists, compared
#   Help             — parameter reference and feature documentation
# =============================================================================

suppressPackageStartupMessages({
  library(wikiliteApp)
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(echarts4r)
  library(htmltools)
  library(jsonlite)
  library(scales)
  library(purrr)
  library(forcats)
  library(tibble)
  library(zoo)
  library(ggalluvial)
  library(stringr)
})

has_DT   <- requireNamespace("DT",       quietly = TRUE)
has_xlsx <- requireNamespace("openxlsx", quietly = TRUE)
if (has_DT)   library(DT)
if (has_xlsx) library(openxlsx)

has_vis  <- requireNamespace("visNetwork", quietly = TRUE)
has_epmc <- requireNamespace("europepmc", quietly = TRUE)
has_cr   <- requireNamespace("rcrossref",  quietly = TRUE)
if (has_vis) library(visNetwork)

# ---------------------------------------------------------------------------
# App-level helpers
# ---------------------------------------------------------------------------

#' Fetch raw wikitext for a specific revision ID from the MediaWiki API.
#'
#' Uses `httr` with a descriptive User-Agent (per MediaWiki API etiquette;
#' generic R UAs get rate-limited aggressively). Honours `Retry-After`
#' on 429 responses, and otherwise retries up to `max_tries` times with
#' exponential backoff + small jitter on transient failures (timeouts,
#' 5xx). Always returns a character scalar or NULL — never throws.
fetch_wikitext_for_revid <- function(revid, lang = "en",
                                     max_tries = 5L, base_sleep = 0.8) {
  if (is.null(revid) || !nzchar(as.character(revid))) return(NULL)

  url <- sprintf(
    paste0("https://%s.wikipedia.org/w/api.php?action=query",
           "&prop=revisions&revids=%s&rvprop=content&rvslots=main",
           "&format=json&formatversion=2"),
    lang, revid
  )

  ua_str <- "wikiliteApp/0.1 (https://github.com/jsobel1/wikiliteApp; jsobel83@gmail.com) R-httr"
  use_httr <- requireNamespace("httr", quietly = TRUE)

  parse_body <- function(body) {
    parsed <- tryCatch(
      jsonlite::fromJSON(body, simplifyDataFrame = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) return(NULL)
    tryCatch(parsed$query$pages[[1]]$revisions[[1]]$slots$main$content,
             error = function(e) NULL)
  }

  if (!use_httr) {
    # Fallback path: same retry logic as before, just with jitter.
    old_to <- getOption("timeout")
    options(timeout = 15)
    on.exit(options(timeout = old_to), add = TRUE)
    for (attempt in seq_len(max_tries)) {
      body <- tryCatch(
        suppressWarnings(readLines(url, warn = FALSE, encoding = "UTF-8")),
        error = function(e) NULL
      )
      if (!is.null(body)) {
        out <- parse_body(paste(body, collapse = ""))
        if (!is.null(out)) return(out)
      }
      if (attempt < max_tries)
        Sys.sleep(base_sleep * 2^(attempt - 1L) + stats::runif(1, 0, 0.3))
    }
    return(NULL)
  }

  for (attempt in seq_len(max_tries)) {
    resp <- tryCatch(
      httr::GET(url,
                httr::user_agent(ua_str),
                httr::timeout(15)),
      error = function(e) NULL
    )
    if (is.null(resp)) {
      Sys.sleep(base_sleep * 2^(attempt - 1L) + stats::runif(1, 0, 0.3))
      next
    }
    status <- httr::status_code(resp)
    if (status >= 200 && status < 300) {
      body <- httr::content(resp, as = "text", encoding = "UTF-8")
      out  <- parse_body(body)
      if (!is.null(out)) return(out)
      return(NULL)
    }
    if (status == 429L || status >= 500L) {
      ra <- as.numeric(httr::headers(resp)[["retry-after"]])
      sleep_s <- if (!is.na(ra) && ra > 0)
                   min(ra, 30)
                 else
                   base_sleep * 2^(attempt - 1L) + stats::runif(1, 0, 0.4)
      if (attempt < max_tries) Sys.sleep(sleep_s)
      next
    }
    # 4xx other than 429 → not transient, bail out.
    return(NULL)
  }
  NULL
}

#' Extract <ref>…</ref> citation strings from wikitext
extract_cites <- function(wikitext) {
  if (is.null(wikitext) || !nzchar(wikitext)) return(character(0))
  hits <- regmatches(
    wikitext,
    gregexpr("<ref(?:\\s[^>]*)?>.*?</ref>|<ref\\s[^/]*/?>",
             wikitext, perl = TRUE, ignore.case = TRUE)
  )[[1]]
  unique(trimws(gsub("\\s+", " ", hits)))
}

#' Classify a raw citation string into a reference type
cite_type <- function(x) {
  xl <- tolower(x)
  if (grepl("\\{\\{\\s*cite\\s+(journal|article)\\b", xl, perl = TRUE))                  return("Journal")
  if (grepl("\\{\\{\\s*cite\\s+(arxiv|ssrn|biorxiv)\\b", xl, perl = TRUE))               return("Preprint")
  if (grepl("\\{\\{\\s*cite\\s+thesis\\b", xl, perl = TRUE))                             return("Thesis")
  if (grepl("\\{\\{\\s*cite\\s+conference\\b", xl, perl = TRUE))                         return("Conference")
  if (grepl("\\{\\{\\s*cite\\s+(report|press.release)\\b", xl, perl = TRUE))             return("Report")
  if (grepl("\\{\\{\\s*cite\\s+(av.media|episode|podcast|video)\\b", xl, perl = TRUE))   return("Multimedia")
  if (grepl("\\{\\{\\s*cite\\s+(patent|court)\\b", xl, perl = TRUE))                     return("Legal/Patent")
  if (grepl("\\{\\{\\s*cite\\s+(tweet|reddit)\\b", xl, perl = TRUE))                     return("Social Media")
  if (grepl("\\{\\{\\s*cite\\s+(book|encyclop|encyclopaedia|encyclopedia)\\b|\\bisbn\\s*=", xl, perl = TRUE)) return("Book")
  if (grepl("\\{\\{\\s*cite\\s+(news|magazine|newspaper)\\b", xl, perl = TRUE))          return("News/Magazine")
  if (grepl("\\{\\{\\s*cite\\s+web\\b", xl, perl = TRUE))                                return("Web")
  if (grepl("\\{\\{\\s*doi\\b", xl, perl = TRUE))                                        return("DOI")
  if (grepl("https?://", xl, perl = TRUE))                                                return("Web")
  "Other"
}

#' Extract a canonical URL for a citation (DOI → doi.org, URL → direct, ISBN → WorldCat)
cite_url <- function(x) {
  doi <- regmatches(x, regexpr("10\\.\\d{4,}/[-._;()/:a-zA-Z0-9]+", x, perl = TRUE))
  if (length(doi) && nzchar(doi[[1]])) return(paste0("https://doi.org/", doi[[1]]))
  url <- regmatches(x, regexpr("https?://[^\\s|>\"'\\}\\]]+", x, perl = TRUE))
  if (length(url) && nzchar(url[[1]])) return(sub("[.,;)]+$", "", url[[1]]))
  m <- regmatches(x, regexpr("(?i)isbn\\s*[=:]?\\s*([0-9][0-9X -]{8,})", x, perl = TRUE))
  if (length(m) && nzchar(m[[1]])) {
    isbn_clean <- gsub("[^0-9X]", "", toupper(sub("(?i)isbn[^0-9]*", "", m[[1]], perl = TRUE)))
    if (nchar(isbn_clean) >= 10L) return(paste0("https://www.worldcat.org/search?q=isbn:", isbn_clean))
  }
  NULL
}

#' Wrap a display label in an HTML <a> tag if a URL is available
make_cite_link <- function(label, url) {
  lbl <- htmltools::htmlEscape(label)
  if (is.null(url) || !nzchar(url %||% "")) return(lbl)
  paste0('<a href="', url, '" target="_blank" rel="noopener noreferrer">', lbl, ' ↗</a>')
}

#' Shorten a raw citation string to a display label
cite_label <- function(x) {
  doi <- regmatches(x, regexpr("10\\.\\d{4,}/\\S+", x, perl = TRUE))
  if (length(doi) && nzchar(doi)) return(doi[[1]])
  m_last <- regmatches(x, regexpr("\\|\\s*last\\s*=\\s*([^|{}\\n]+)", x, perl = TRUE))
  if (length(m_last) && nzchar(m_last)) {
    last <- trimws(sub("\\|\\s*last\\s*=\\s*", "", m_last[[1]], perl = TRUE))
    m_yr <- regmatches(x, regexpr("\\|\\s*year\\s*=\\s*(\\d{4})", x, perl = TRUE))
    yr   <- if (length(m_yr) && nzchar(m_yr))
               sub(".*=\\s*", "", m_yr[[1]], perl = TRUE)
             else ""
    return(paste0(last, if (nzchar(yr)) paste0(" (", yr, ")") else ""))
  }
  substr(x, 1, 70)
}

#' Convert a #RRGGBB hex colour to "rgba(r,g,b,a)" string
hex_to_rgba <- function(hex, alpha = 0.82) {
  hex <- sub("^#", "", hex)
  sprintf("rgba(%d,%d,%d,%.2f)",
          strtoi(substr(hex, 1, 2), 16L),
          strtoi(substr(hex, 3, 4), 16L),
          strtoi(substr(hex, 5, 6), 16L),
          alpha)
}

#' Sample the rows of a (chronologically ordered) revision history.
#'
#' @param h Data frame of revisions (rows already sorted oldest → newest).
#' @param n Target number of revisions.
#' @param method One of "recent" (keep last n), "even" (evenly spaced across
#'   range, retaining first and last), or "random" (random subset, retaining
#'   first and last for a stable timeline). When n >= nrow(h), returns h.
#' @return Sub-sampled data frame, still in chronological order.
sample_revisions <- function(h, n, method = c("recent", "even", "random")) {
  method <- match.arg(method)
  total  <- nrow(h)
  if (!is.numeric(n) || length(n) != 1L || is.na(n)) return(h)
  n <- as.integer(max(2L, round(n)))
  if (total <= n) return(h)

  idx <- switch(
    method,
    recent = seq.int(total - n + 1L, total),
    even   = unique(round(seq(1, total, length.out = n))),
    random = {
      mid <- if (n > 2L) sort(sample.int(total - 2L, n - 2L)) + 1L else integer(0)
      unique(c(1L, mid, total))
    }
  )
  h[idx, , drop = FALSE]
}

#' Fetch token-level authorship for a single revision from the WikiWho API.
#'
#' Returns a data frame with one row per token: `str` (the token text),
#' `editor` (a userid string, or `0|<IP>` for anonymous edits) and
#' `o_rev_id` (the revision in which the token was first introduced).
#' Returns NULL on any failure — never throws.
#'
#' API host is the maintained WMCloud mirror (the original
#' api.wikiwho.net is dead).
fetch_wikiwho_tokens <- function(article, revid, lang = "en",
                                 max_tries = 3L, base_sleep = 0.8) {
  if (!nzchar(article) || !nzchar(as.character(revid))) return(NULL)
  if (!requireNamespace("httr", quietly = TRUE)) return(NULL)

  url <- sprintf(
    "https://wikiwho-api.wmcloud.org/%s/api/v1.0.0-beta/rev_content/%s/%s/?editor=true&token_id=true&o_rev_id=true",
    lang,
    utils::URLencode(article, reserved = TRUE),
    utils::URLencode(as.character(revid), reserved = TRUE)
  )
  ua <- "wikiliteApp/0.1 (https://github.com/jsobel1/wikiliteApp; jsobel83@gmail.com) R-httr"

  for (attempt in seq_len(max_tries)) {
    resp <- tryCatch(
      httr::GET(url, httr::user_agent(ua), httr::timeout(45)),
      error = function(e) NULL
    )
    if (is.null(resp)) {
      if (attempt < max_tries) Sys.sleep(base_sleep * 2^(attempt - 1L))
      next
    }
    code <- httr::status_code(resp)
    if (code == 429L || code >= 500L) {
      if (attempt < max_tries) Sys.sleep(base_sleep * 2^(attempt - 1L))
      next
    }
    if (code != 200L) return(NULL)
    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE),
                       error = function(e) NULL)
    if (is.null(parsed) || !isTRUE(parsed$success)) return(NULL)
    revs <- parsed$revisions
    if (!length(revs)) return(NULL)
    rev_obj <- revs[[1]][[1]]
    toks <- rev_obj$tokens
    if (!length(toks)) return(NULL)

    df <- data.frame(
      str      = vapply(toks, function(t) as.character(t$str %||% ""), character(1L)),
      editor   = vapply(toks, function(t) as.character(t$editor %||% ""), character(1L)),
      o_rev_id = vapply(toks, function(t) as.character(t$o_rev_id %||% ""), character(1L)),
      stringsAsFactors = FALSE
    )
    return(df)
  }
  NULL
}

#' Build coloured-by-author HTML from WikiWho tokens.
#'
#' Each token becomes a span coloured by its first-author. Consecutive
#' tokens with the same author are merged into a single span for compact
#' rendering. Returns the same shape as [build_line_attribution()] so the
#' Shiny UI can consume either drop-in.
build_wikiwho_attribution <- function(tokens_df, hist, pal, user_label_map) {
  if (is.null(tokens_df) || nrow(tokens_df) == 0L) return(NULL)

  # Map o_rev_id -> author_label using local hist when possible.
  rev_to_user  <- setNames(hist$user, as.character(hist$revid))
  rev_to_label <- setNames(hist$author_label, as.character(hist$revid))

  is_ip_editor <- startsWith(tokens_df$editor, "0|")
  ip_str       <- ifelse(is_ip_editor, sub("^0\\|", "", tokens_df$editor), NA_character_)

  # Resolve label per token: anonymous IP, then o_rev_id lookup, then fallback.
  labels <- character(nrow(tokens_df))
  users  <- character(nrow(tokens_df))
  for (i in seq_len(nrow(tokens_df))) {
    if (is_ip_editor[i]) {
      users[i]  <- ip_str[i]
      labels[i] <- "Anonymous"
    } else {
      orev <- tokens_df$o_rev_id[i]
      lbl  <- rev_to_label[[orev]] %||% NA_character_
      usr  <- rev_to_user[[orev]]  %||% NA_character_
      if (!is.null(lbl) && !is.na(lbl)) {
        users[i]  <- if (!is.na(usr)) usr else paste0("user#", tokens_df$editor[i])
        labels[i] <- lbl
      } else {
        users[i]  <- paste0("user#", tokens_df$editor[i])
        labels[i] <- "Other (registered)"
      }
    }
  }

  # Run-length encode by label so consecutive same-author tokens collapse.
  rle_lab <- rle(labels)
  starts  <- cumsum(c(1L, head(rle_lab$lengths, -1)))
  spans   <- character(length(rle_lab$lengths))

  no_left_space  <- function(t) grepl("^[\\.,;:!\\?\\)\\]\\}>%]", t, perl = TRUE)
  no_right_space <- function(t) grepl("[\\(\\[\\{<]$", t, perl = TRUE)

  for (k in seq_along(rle_lab$lengths)) {
    s_idx <- starts[k]
    e_idx <- s_idx + rle_lab$lengths[k] - 1L
    toks  <- tokens_df$str[s_idx:e_idx]
    pieces <- character(length(toks))
    for (j in seq_along(toks)) {
      sep <- if (j == 1L) ""
             else if (no_left_space(toks[j]) || no_right_space(toks[j - 1L])) ""
             else " "
      pieces[j] <- paste0(sep, htmltools::htmlEscape(toks[j]))
    }
    txt   <- paste(pieces, collapse = "")
    lbl   <- rle_lab$values[k]
    col   <- pal[[lbl]] %||% "#CCCCCC"
    bg    <- hex_to_rgba(col, 0.22)
    title <- sprintf("%s · token %d–%d (rev %s)",
                     lbl, s_idx, e_idx, tokens_df$o_rev_id[s_idx])
    spans[k] <- sprintf(
      '<span style="background:%s; padding:0 2px; border-radius:2px;" title="%s">%s</span>',
      bg, htmltools::htmlEscape(title), txt
    )
  }

  # Per-author summary table
  summary_df <- data.frame(label = labels, stringsAsFactors = FALSE) %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(n_tokens = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n_tokens)) %>%
    dplyr::mutate(color = vapply(label,
                                 function(l) pal[[l]] %||% "#CCCCCC",
                                 character(1L))) %>%
    as.data.frame()

  list(
    html       = paste(spans, collapse = " "),
    n_snaps    = nrow(tokens_df),    # used by the UI label as "n tokens"
    summary_df = summary_df,
    source     = "wikiwho"
  )
}

#' Colour wikitext lines by first-appearance editor (local diff approach)
build_line_attribution <- function(target_wikitext, snap_rows,
                                   pal, user_label_map, lang = "en") {
  if (is.null(target_wikitext) || !nzchar(target_wikitext)) return(NULL)

  target_lines <- strsplit(target_wikitext, "\n", fixed = TRUE)[[1]]
  if (length(target_lines) == 0) return(NULL)

  n_snaps <- nrow(snap_rows)

  snap_sets <- lapply(seq_len(n_snaps), function(i) {
    wt <- tryCatch(fetch_wikitext_for_revid(snap_rows$revid[i], lang),
                   error = function(e) NULL)
    if (is.null(wt)) return(NULL)
    lns <- strsplit(wt, "\n", fixed = TRUE)[[1]]
    as.list(setNames(rep(TRUE, length(lns)), lns))
  })

  html_lines   <- character(length(target_lines))
  line_authors <- character(length(target_lines))
  line_dates   <- as.POSIXct(rep(NA_real_, length(target_lines)), origin = "1970-01-01", tz = "UTC")

  for (i in seq_along(target_lines)) {
    line <- target_lines[i]

    if (!nzchar(trimws(line))) {
      html_lines[i] <- htmltools::htmlEscape(line)
      next
    }

    author   <- snap_rows$user[n_snaps]
    first_ts <- snap_rows$ts_posix[n_snaps]

    for (j in seq_len(n_snaps)) {
      s <- snap_sets[[j]]
      if (!is.null(s) && isTRUE(s[[line]])) {
        author   <- snap_rows$user[j]
        first_ts <- snap_rows$ts_posix[j]
        break
      }
    }

    line_authors[i] <- author
    line_dates[i]   <- first_ts

    lbl <- user_label_map[[author]] %||% author
    col <- pal[[lbl]] %||% "#CCCCCC"
    bg  <- hex_to_rgba(col, 0.22)
    html_lines[i] <- sprintf(
      '<span style="display:block;background:%s;padding:0 4px;border-radius:2px;" title="%s · first seen %s">%s</span>',
      bg, htmltools::htmlEscape(lbl),
      format(first_ts, "%Y-%m-%d"),
      htmltools::htmlEscape(line)
    )
  }

  keep       <- nzchar(line_authors)
  auth_users <- line_authors[keep]
  auth_dates <- line_dates[keep]

  summary_df <- if (length(auth_users) > 0) {
    df <- data.frame(user = auth_users, ts = auth_dates, stringsAsFactors = FALSE)
    df %>%
      group_by(user) %>%
      summarise(n_lines   = n(),
                first_ins = min(ts, na.rm = TRUE),
                .groups   = "drop") %>%
      mutate(
        label = vapply(user, function(u) user_label_map[[u]] %||% u, character(1L)),
        color = vapply(label, function(l) pal[[l]] %||% "#CCCCCC", character(1L))
      ) %>%
      arrange(first_ins)
  } else {
    NULL
  }

  list(html       = paste(html_lines, collapse = "\n"),
       n_snaps    = n_snaps,
       summary_df = summary_df)
}

#' Compute line-level diff between two wikitext strings (set-based).
compute_line_diff <- function(old_text, new_text) {
  if (is.null(old_text) || is.null(new_text)) return(NULL)

  split_nonblank <- function(txt) {
    lns <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    lns[nzchar(trimws(lns))]
  }
  old_lines <- split_nonblank(old_text)
  new_lines <- split_nonblank(new_text)

  removed   <- setdiff(old_lines, new_lines)
  added     <- setdiff(new_lines, old_lines)
  unchanged <- length(intersect(old_lines, new_lines))

  list(removed     = removed,
       added       = added,
       n_removed   = length(removed),
       n_added     = length(added),
       n_unchanged = unchanged)
}

# ---------------------------------------------------------------------------
# CSS
# ---------------------------------------------------------------------------

app_css <- "
body { font-family: 'Segoe UI', Arial, sans-serif; font-size: 13px; }

/* Top navbar */
.navbar { margin-bottom: 0; }
.navbar-default { background:#2c3e50; border-color:#1a252f; }
.navbar-default .navbar-brand,
.navbar-default .navbar-nav > li > a { color:#ecf0f1; }
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus { background:#1a252f; color:#fff; }
.navbar-default .navbar-nav > li > a:hover { background:#34495e; color:#fff; }

/* Section headers */
.section-title { font-size:15px; font-weight:600; color:#2c3e50;
  margin:0 0 8px 0; padding-bottom:4px; border-bottom:1px solid #e0e4e8; }
.section-sub { font-size:12px; color:#7f8c8d; margin-bottom:10px; }

/* Control bar (legacy horizontal, kept for back-compat) */
.ctrl-bar { background:#f0f2f5; padding:10px 16px 8px; border-bottom:1px solid #d0d3d8;
  margin-bottom:14px; }
.ctrl-bar .form-group { margin-bottom:0; }
.ctrl-bar .control-label { font-size:11px; font-weight:600; color:#555;
  margin-bottom:2px; text-transform:uppercase; letter-spacing:0.03em; }

/* Single-article control sidebar */
.ctrl-side { background:#f0f2f5; border:1px solid #d0d3d8; border-radius:5px;
  padding:12px 12px 10px; margin:10px 0 10px; position:sticky; top:60px; }
.ctrl-side .form-group { margin-bottom:6px; }
.ctrl-side .control-label { font-size:11px; font-weight:600; color:#555;
  margin-bottom:2px; text-transform:uppercase; letter-spacing:0.03em; }
.ctrl-side .selectize-input,
.ctrl-side .form-control { font-size:12px; }

/* Side panels */
.side-panel { background:#fafbfc; border:1px solid #e8eaed; border-radius:4px;
  padding:10px 12px; }
.panel-hdr { font-size:12.5px; font-weight:600; color:#444; margin:0 0 4px 0; }

/* Author chips */
.author-chip { display:flex; align-items:center; gap:5px;
  padding:3px 5px; margin-bottom:2px; border-radius:3px;
  font-size:11.5px; line-height:1.3; }
.author-chip:hover { background:#eef; }
.swatch { width:11px; height:11px; border-radius:2px; flex-shrink:0; }
.aname  { flex:1; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.acount { color:#888; font-size:10px; white-space:nowrap; min-width:22px; text-align:right; }

/* Revision text & diff */
.rev-meta { font-size:11px; color:#666; margin:2px 0 5px 0; min-height:1.4em; }
.wikibox { height:480px; overflow-y:auto; font-size:11px;
  font-family:'Consolas',monospace; white-space:pre-wrap; word-break:break-word;
  background:#fafafa; border:1px solid #ddd; padding:8px 10px;
  border-radius:3px; line-height:1.5; }
.wikibox mark { background:#fffacd; border-radius:2px; }
.diff-box { height:480px; overflow-y:auto; font-size:11px;
  font-family:'Consolas',monospace; white-space:pre-wrap; word-break:break-word;
  background:#fafafa; border:1px solid #ddd; padding:8px 10px;
  border-radius:3px; line-height:1.5; }
.diff-added   { display:block; background:#e6ffed; color:#22863a; padding:0 4px; }
.diff-removed { display:block; background:#ffeef0; color:#b31d28; padding:0 4px; }
.diff-note    { color:#999; font-size:10px; font-style:italic; padding:2px 4px; }
.attr-table { width:100%; border-collapse:collapse; font-size:10.5px; margin-top:6px; }
.attr-table th { background:#f0f2f5; padding:3px 5px; text-align:left;
  font-weight:600; border-bottom:1px solid #ddd; }
.attr-table td { padding:2px 5px; border-bottom:1px solid #f0f0f0; vertical-align:middle; }
.attr-swatch { display:inline-block; width:10px; height:10px; border-radius:2px;
  margin-right:4px; vertical-align:middle; }

/* Color picker */
input[type='color'].color-pick { width:14px; height:14px; border:none;
  padding:0; cursor:pointer; border-radius:2px; flex-shrink:0;
  vertical-align:middle; -webkit-appearance:none; }
input[type='color'].color-pick::-webkit-color-swatch-wrapper { padding:0; }
input[type='color'].color-pick::-webkit-color-swatch { border:none; border-radius:2px; }

/* Group chip (flow chart authors) */
.grp-chip { display:flex; align-items:center; gap:4px; padding:2px 4px;
  margin-bottom:2px; border-radius:3px; font-size:11.5px; }
.grp-chip:hover { background:#eef; }
.grp-chip.disabled { opacity:0.4; }

/* Metric cards */
.metric-row { display:flex; flex-wrap:wrap; gap:8px; margin-bottom:10px; }
.metric-card { display:flex; flex-direction:column; align-items:center;
  background:#f8f9fa; border:1px solid #e9ecef; border-radius:6px;
  padding:7px 14px; min-width:90px; }
.metric-val { font-size:20px; font-weight:700; color:#2c3e50; line-height:1.2; }
.metric-lbl { font-size:9.5px; color:#888; margin-top:2px;
  text-transform:uppercase; letter-spacing:0.04em; }

/* Misc */
.wiki-art-link { font-size:12px; margin:0 0 4px 2px; }
.wiki-art-link a { color:#0645ad; text-decoration:none; }
.wiki-art-link a:hover { text-decoration:underline; }
.dl-bar { display:flex; align-items:center; gap:6px; margin-bottom:6px; }
.sci-bar-wrap { height:6px; background:#e8e8e8; border-radius:3px;
  margin-top:4px; width:100%; }
.sci-bar-fill { height:6px; border-radius:3px; }
.latency-section { margin-top:10px; padding-top:8px; border-top:1px solid #e8e8e8; }

/* Corpus chips */
.corpus-card { background:#fafbfc; border:1px solid #e8eaed; border-radius:4px;
  padding:8px 10px; margin-bottom:8px; }
.corpus-card .name { font-weight:600; color:#2c3e50; font-size:13px; }
.corpus-card .meta { font-size:11px; color:#7f8c8d; margin-top:2px; }
.corpus-card .arts { font-size:10.5px; color:#555; max-height:60px;
  overflow-y:auto; margin-top:4px; }
.corpus-card .actions { margin-top:6px; }

/* Help */
.help-section { padding:6px 0; }
.help-section h4 { color:#2c3e50; margin-top:14px; margin-bottom:6px; }
.help-section p, .help-section li { font-size:13px; line-height:1.55; color:#333; }
.help-param { font-family:'Consolas',monospace; background:#f0f2f5;
  padding:1px 4px; border-radius:2px; font-size:12px; }
"

# ---------------------------------------------------------------------------
# UI fragments
# ---------------------------------------------------------------------------

# ── Single-article control sidebar ─────────────────────────────────────────
single_ctrl_sidebar <- div(
  class = "ctrl-side",
  p(class = "section-title", style = "margin-top:0;", "Parameters"),

  tags$div(
    tags$label("Article", `for` = "article",
               class = "control-label", style = "display:block;"),
    selectizeInput(
      "article", label = NULL,
      choices  = c("Zeitgeber"),
      selected = "Zeitgeber",
      width    = "100%",
      options  = list(
        placeholder  = "Type to search Wikipedia…",
        loadThrottle = 350,
        create       = TRUE,
        createOnBlur = TRUE,
        maxOptions   = 12,
        load = I("function(query, callback) {
          if (!query.length) return callback();
          $.ajax({
            url: 'https://en.wikipedia.org/w/api.php',
            data: { action:'opensearch', search:query, limit:12,
                    namespace:0, format:'json', origin:'*' },
            type: 'GET', dataType:'json',
            error: function() { callback(); },
            success: function(res) {
              callback(res[1].map(function(t){ return {value:t,label:t}; }));
            }
          });
        }")
      )
    )
  ),

  fluidRow(
    column(6, dateInput("date_from", "From", value = "2001-01-01",
                        min = "2001-01-01", width = "100%")),
    column(6, dateInput("date_to",   "To",   value = Sys.Date(), width = "100%"))
  ),

  fluidRow(
    column(6, numericInput("min_delta", "Min |Δ| B",
                           value = 0, min = 0, max = 100000,
                           step = 50, width = "100%")),
    column(6, numericInput("n_top", "Top N",
                           value = 8, min = 4, max = 15, step = 1,
                           width = "100%"))
  ),

  fluidRow(
    column(7, numericInput("sample_n", "Sample size",
                           value = 1500, min = 50, max = 10000,
                           step = 100, width = "100%")),
    column(5, selectInput("sample_method", "Method",
                          choices = c("Most recent" = "recent",
                                      "Even"        = "even",
                                      "Random"      = "random"),
                          selected = "recent", width = "100%"))
  ),

  actionButton("go", "Fetch →",
               class = "btn-primary",
               style = "width:100%; padding:6px; margin-top:6px;")
)

# ── Help content ───────────────────────────────────────────────────────────
help_panel <- div(
  class = "help-section",
  style = "max-width:900px; margin:0 auto; padding:20px;",
  h3("wikiliteApp — User guide"),
  p("This app explores Wikipedia article revision histories: who wrote what, when, ",
    "how the article evolved, what citations were used, and signs of vandalism or edit wars. ",
    "Pick a mode in the navigation bar at the top:"),
  tags$ul(
    tags$li(tags$b("Single Article"), " — full multi-tab analysis of one article."),
    tags$li(tags$b("Corpus Analysis"), " — define one or more named, optionally category-",
            "linked article lists, and compare them.")
  ),

  h4("Control bar parameters (Single Article)"),
  tags$ul(
    tags$li(tags$span(class = "help-param", "Article"),
            " — type to search Wikipedia (live suggestions). Press Enter to keep a custom title."),
    tags$li(tags$span(class = "help-param", "From / To"),
            " — date range filter applied after fetching the full history."),
    tags$li(tags$span(class = "help-param", "Min |Δ| B"),
            " — drop revisions whose absolute byte change vs the previous revision ",
            "is below this threshold. Useful for hiding tiny typo fixes."),
    tags$li(tags$span(class = "help-param", "Top N"),
            " — number of named editor groups to keep distinct in the History Flow chart. ",
            "Smaller editors are merged into ", tags$em("Other"), "."),
    tags$li(tags$span(class = "help-param", "Sample size"),
            " — for very long histories the analysis is capped at this many revisions."),
    tags$li(tags$span(class = "help-param", "Method"),
            " — how to choose which revisions to keep when sampling: ",
            tags$b("Most recent"), " (last N within the date range; default), ",
            tags$b("Even"), " (evenly spaced across the date range — best for long-term overviews), ",
            tags$b("Random"), " (random subset, keeping the first and last).")
  ),

  h4("Single-article tabs"),
  tags$ul(
    tags$li(tags$b("History Flow"),
            " — Sankey-like ribbon diagram showing which editors own which slice ",
            "of the article over time. Click a node to load that revision in the ",
            "Revision Inspector tab."),
    tags$li(tags$b("Citations"),
            " — fetches wikitext at N evenly spaced snapshots, extracts ",
            tags$code("<ref>"), " citations, classifies them (Journal, Book, Web, …), ",
            "shows their lifespan, and computes a SciScore (% journal references, ",
            "% DOI-linked). Optionally annotate via EuropePMC and compute citation ",
            "latency (delay between publication and citation)."),
    tags$li(tags$b("Authorship"),
            " — anonymous vs registered share over time, top editors, cumulative edit curve, ",
            "Gini inequality, full editor table with revert rates."),
    tags$li(tags$b("Stability"),
            " — article size timeline, distribution of byte deltas, editing rhythm by ",
            "time-of-day / day-of-week."),
    tags$li(tags$b("Vandalism & Wars"),
            " — heuristic detection of vandalism events (rapid revert pairs), survival ",
            "time of vandal edits, and edit-war episodes (clusters of revert/counter-revert)."),
    tags$li(tags$b("Revision Inspector"),
            " — pick any individual revision; see its raw wikitext coloured by line ",
            "attribution (which editor first introduced each line) and the line-level ",
            "diff against the previous revision.")
  ),

  h4("Corpus Analysis"),
  p("A ", tags$b("corpus"), " is a named list of articles, optionally tagged with a ",
    "Wikipedia category label. You can have several corpora open at once and compare them."),
  tags$ul(
    tags$li("Add a corpus by typing a name and pasting article titles, one per line."),
    tags$li("Or fetch from a Wikipedia category — its articles auto-populate, and the ",
            "category name is used as the corpus label."),
    tags$li("All corpora appear in the cross-corpus tabs: ",
            tags$b("Timeline"), ", ",
            tags$b("Co-citation network"), ", ",
            tags$b("Publication network"), ", ",
            tags$b("Wikilink network"), ".")
  ),

  h4("SciScore"),
  p("Two summary citation-quality metrics:"),
  tags$ul(
    tags$li(tags$b("Journal %"), " — share of currently active citations classified as ",
            tags$em("Journal"), " (peer-reviewed)."),
    tags$li(tags$b("DOI coverage %"), " — share of currently active citations that ",
            "carry a DOI (regardless of type).")
  ),

  h4("Notes"),
  tags$ul(
    tags$li("All API calls are rate-limited and retried defensively. Long fetches show a ",
            "progress notification."),
    tags$li("XLSX downloads (citations, editor table, vandalism events, edit wars, ",
            "revision history) appear in each tab when the ", tags$code("openxlsx"),
            " package is installed."),
    tags$li("BibTeX export is available when ", tags$code("rcrossref"), " is installed."),
    tags$li("Network plots require the ", tags$code("visNetwork"), " package.")
  )
)

# ── UI ─────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "wikiliteApp",
  id    = "main_nav",
  collapsible = TRUE,
  header = tagList(
    tags$head(
      tags$style(HTML(app_css)),
      tags$title("Wikipedia History Flow")
    )
  ),

  # ════════════════════════════════════════════════════════════════════════
  # SINGLE ARTICLE
  # ════════════════════════════════════════════════════════════════════════
  tabPanel(
    "Single Article",
    value = "single",

    fluidRow(
      column(2, style = "padding-left:14px; padding-right:6px;",
        single_ctrl_sidebar
      ),
      column(10, style = "padding-left:6px; padding-right:14px;",
        uiOutput("wiki_link_ui"),

        tabsetPanel(
          id   = "single_tabs",
          type = "tabs",

        # ── History Flow ────────────────────────────────────────────────
        tabPanel(
          "History Flow",
          br(),
          fluidRow(
            column(2,
              div(class = "side-panel",
                p(class = "panel-hdr", "Authors"),
                div(style = "max-height:600px; overflow-y:auto;",
                    uiOutput("author_list"))
              )
            ),
            column(10,
              echarts4rOutput("hf_plot", height = "560px", width = "100%")
            )
          )
        ),

        # ── Citations ───────────────────────────────────────────────────
        tabPanel(
          "Citations",
          br(),
          fluidRow(
            column(3,
              selectInput("cite_display", "Status",
                          choices = c("Active in latest revision",
                                      "All citations",
                                      "Additions only (not in v1)",
                                      "Removed only"),
                          selected = "Active in latest revision",
                          width = "100%")
            ),
            column(3,
              selectInput("cite_type_filter", "Reference type",
                          choices = c("All types", "Journal", "Book", "Web",
                                      "News/Magazine", "Preprint", "Thesis",
                                      "Conference", "Report", "Multimedia",
                                      "Legal/Patent", "Social Media", "DOI", "Other"),
                          width = "100%")
            ),
            column(2,
              numericInput("cite_n_snapshots", "# snapshots",
                           value = 15, min = 3, max = 30, step = 1,
                           width = "100%")
            ),
            column(2,
              br(),
              actionButton("analyse_citations", "Analyse",
                           class = "btn-info", style = "width:100%;")
            ),
            column(2,
              br(),
              if (has_xlsx)
                downloadButton("dl_cite_xlsx", "⬇ XLSX",
                               class = "btn-sm btn-default", style = "width:100%;"),
              if (has_cr)
                downloadButton("dl_bibtex", "⬇ BibTeX",
                               class = "btn-sm btn-default",
                               style = "width:100%; margin-top:4px;")
            )
          ),
          fluidRow(
            column(4,
              actionButton("annotate_epmc", "Annotate DOIs with EuropePMC",
                           class = "btn-sm btn-default", style = "width:100%;")
            ),
            column(4,
              actionButton("annotate_isbn", "Annotate ISBNs with Google Books",
                           class = "btn-sm btn-default", style = "width:100%;")
            ),
            column(4,
              if (has_xlsx) tagList(
                tags$div(style = "display:flex; gap:4px;",
                  selectInput("export_type", label = NULL,
                              choices  = c("All filtered", "Journal", "Book", "Web",
                                           "News/Magazine", "Preprint", "Thesis",
                                           "Conference", "Report", "Multimedia",
                                           "Legal/Patent", "Social Media", "DOI", "Other"),
                              selected = "Journal", width = "60%"),
                  downloadButton("dl_cite_by_type", "⬇ XLSX (type)",
                                 class = "btn-sm btn-default", style = "width:40%;")
                )
              )
            )
          ),
          uiOutput("sci_cards"),
          fluidRow(
            column(7,
              p(class = "panel-hdr", "Citation timeline"),
              div(style = "height:380px; overflow-y:auto; border:1px solid #e8e8e8; border-radius:3px;",
                  plotlyOutput("cite_plot", height = "auto"))
            ),
            column(5,
              p(class = "panel-hdr", "Citation type counts"),
              plotlyOutput("cite_type_count_plot", height = "380px")
            )
          ),
          br(),
          if (has_DT) DT::DTOutput("cite_table") else tableOutput("cite_table_base"),
          div(class = "latency-section",
            div(class = "dl-bar",
                actionButton("compute_latency",
                             "Compute Citation Latency (EuropePMC)",
                             class = "btn-sm btn-warning")
            ),
            fluidRow(
              column(6, plotOutput("latency_dist_plot", height = "270px")),
              column(6, plotOutput("latency_seg_plot",  height = "270px"))
            )
          )
        ),

        # ── Authorship ──────────────────────────────────────────────────
        tabPanel(
          "Authorship",
          br(),
          uiOutput("auth_cards"),
          fluidRow(
            column(6, plotOutput("auth_anon_plot",  height = "270px")),
            column(6, plotOutput("auth_top_plot",   height = "270px"))
          ),
          plotlyOutput("auth_cum_plot", height = "220px"),
          br(),
          div(class = "dl-bar",
              tags$b("Editor breakdown"),
              if (has_xlsx)
                downloadButton("dl_editors_xlsx", "⬇ XLSX", class = "btn-sm btn-default")
          ),
          if (has_DT) DT::DTOutput("editor_tbl") else tableOutput("editor_tbl_base")
        ),

        # ── Stability ───────────────────────────────────────────────────
        tabPanel(
          "Stability",
          br(),
          uiOutput("stab_cards"),
          plotlyOutput("stab_size_plot",  height = "270px"),
          fluidRow(
            column(6, plotOutput("stab_delta_plot",  height = "240px")),
            column(6, plotOutput("stab_rhythm_plot", height = "240px"))
          ),
          br(),

          # ── Citation-quality (SciScore) sub-section ──────────────────
          tags$hr(),
          p(class = "section-title",
            "Citation quality (SciScore) over time"),
          p(class = "section-sub",
            "Per-snapshot share of journal references and DOI-linked references ",
            "among citations active at that snapshot. Run the Citations tab first ",
            "to populate this section."),
          uiOutput("stab_sci_cards"),
          plotlyOutput("stab_sci_timeline_plot", height = "280px"),
          br(),
          uiOutput("stab_sci_type_mix_plot_ui"),
          br(),

          div(class = "dl-bar",
              if (has_xlsx)
                downloadButton("dl_history_xlsx", "⬇ Revision history XLSX",
                               class = "btn-sm btn-default"),
              if (has_xlsx)
                downloadButton("dl_sci_timeline_xlsx", "⬇ SciScore timeline XLSX",
                               class = "btn-sm btn-default")
          )
        ),

        # ── Vandalism & Wars ────────────────────────────────────────────
        tabPanel(
          "Vandalism & Wars",
          br(),
          uiOutput("van_cards"),
          plotlyOutput("van_timeline_plot", height = "290px"),
          fluidRow(
            column(6,
              plotOutput("van_survival_plot", height = "220px"),
              div(class = "dl-bar",
                  if (has_xlsx)
                    downloadButton("dl_vandalism_xlsx", "⬇ Events XLSX",
                                   class = "btn-sm btn-default")
              )
            ),
            column(6,
              plotlyOutput("war_plot", height = "220px")
            )
          ),
          br(),
          div(class = "dl-bar",
              tags$b("Edit war episodes"),
              if (has_xlsx)
                downloadButton("dl_wars_xlsx", "⬇ XLSX", class = "btn-sm btn-default")
          ),
          if (has_DT) DT::DTOutput("war_tbl") else tableOutput("war_tbl_base")
        ),

        # ── Revision Inspector (Text + Changes) ─────────────────────────
        tabPanel(
          "Revision Inspector",
          br(),
          fluidRow(
            column(3,
              div(class = "side-panel",
                p(class = "panel-hdr", "Revision"),
                selectInput("rev_selector", label = NULL,
                            choices  = character(0),
                            width    = "100%"),
                uiOutput("rev_meta")
              )
            ),
            column(5,
              p(class = "panel-hdr", "Text (coloured by author)"),
              uiOutput("wikitext_panel")
            ),
            column(4,
              p(class = "panel-hdr", "Changes vs. previous revision"),
              uiOutput("diff_panel")
            )
          )
        )
        )  # close tabsetPanel
      )    # close column(10)
    )      # close fluidRow (single-article 2-column layout)
  ),

  # ════════════════════════════════════════════════════════════════════════
  # CORPUS ANALYSIS
  # ════════════════════════════════════════════════════════════════════════
  tabPanel(
    "Corpus Analysis",
    value = "corpus",

    div(style = "padding:14px 16px 0;",

      fluidRow(
        # Left: corpus management
        column(4,
          div(class = "side-panel",
            p(class = "section-title", "Corpora"),
            p(class = "section-sub",
              "A corpus is a named list of articles, optionally tied to a Wikipedia category. ",
              "Add as many as you like — they appear together in the comparison tabs."),

            uiOutput("corpus_list_ui"),

            tags$hr(),
            p(class = "panel-hdr", "Add new corpus"),
            textInput("new_corpus_name", "Name / category label",
                      placeholder = "e.g. Circadian rhythm", width = "100%"),
            textAreaInput("new_corpus_articles", "Articles (one per line)",
                          rows = 4, width = "100%",
                          placeholder = "Zeitgeber\nCircadian rhythm\nSleep deprivation"),
            fluidRow(
              column(7,
                textInput("new_corpus_cat", "Or fetch from Wikipedia category",
                          placeholder = "e.g. Circadian rhythm", width = "100%")
              ),
              column(5,
                selectInput("new_corpus_lang", "Lang",
                            choices = c("en","fr","de","es","it","pt","nl","ru","ja","zh"),
                            selected = "en", width = "100%")
              )
            ),
            fluidRow(
              column(6,
                actionButton("corpus_fetch_cat", "Fetch category →",
                             class = "btn-default btn-sm", style = "width:100%;")
              ),
              column(6,
                actionButton("corpus_add", "Add corpus",
                             class = "btn-primary btn-sm", style = "width:100%;")
              )
            ),

            tags$hr(style = "margin:14px 0 10px;"),
            actionButton("corpus_render",
                         "Render comparison →",
                         class = "btn-info btn-block",
                         style = "padding:8px; font-weight:600;"),
            tags$div(style = "font-size:11px; color:#7f8c8d; margin-top:4px;",
                     "Click after editing corpora to refresh the panels on the right.")
          )
        ),

        # Right: comparison tabs
        column(8,
          div(style = "padding-left:6px;",
            p(class = "section-title", "Cross-corpus comparison"),
            uiOutput("corpus_summary"),

            tabsetPanel(
              id = "corpus_subtabs",
              tabPanel("Timeline",
                br(),
                fluidRow(
                  column(4,
                    selectInput("corpus_timeline_y", "Y-axis grouping",
                                choices = c("By corpus" = "corpus",
                                            "All articles" = "article"),
                                selected = "corpus", width = "100%")
                  )
                ),
                plotlyOutput("corpus_timeline", height = "560px")
              ),

              # ── Citations ────────────────────────────────────────────
              tabPanel("Citations",
                br(),
                fluidRow(
                  column(4,
                    actionButton("corpus_extract_cites",
                                 "Extract citations from corpus",
                                 class = "btn-info", style = "width:100%;")
                  ),
                  column(4,
                    actionButton("corpus_annotate_epmc",
                                 "Annotate DOIs (EuropePMC)",
                                 class = "btn-sm btn-default",
                                 style = "width:100%;")
                  ),
                  column(4,
                    actionButton("corpus_annotate_isbn",
                                 "Annotate ISBNs (Google Books)",
                                 class = "btn-sm btn-default",
                                 style = "width:100%;")
                  )
                ),
                if (has_xlsx) fluidRow(
                  column(4),  # spacer to align with the buttons above
                  column(4,
                    downloadButton("dl_corpus_epmc_xlsx",
                                   "⬇ Annotated DOIs XLSX",
                                   class = "btn-sm btn-default",
                                   style = "width:100%; margin-top:4px;")
                  ),
                  column(4,
                    downloadButton("dl_corpus_isbn_xlsx",
                                   "⬇ Annotated ISBNs XLSX",
                                   class = "btn-sm btn-default",
                                   style = "width:100%; margin-top:4px;")
                  )
                ),
                br(),
                uiOutput("corpus_sci_cards"),
                fluidRow(
                  column(7,
                    p(class = "panel-hdr",
                      "Reference composition (citation types)"),
                    fluidRow(
                      column(6,
                        radioButtons("corpus_type_view", label = NULL,
                          choices = c("Per corpus (stacked)" = "corpus",
                                      "Per article (stacked)" = "article"),
                          selected = "corpus", inline = TRUE)
                      ),
                      column(6,
                        conditionalPanel(
                          "input.corpus_type_view == 'article'",
                          fluidRow(
                            column(7,
                              selectInput("corpus_type_normalise",
                                          label = NULL,
                                          choices = c("Counts"   = "counts",
                                                      "% (within article)" = "pct"),
                                          selected = "counts", width = "100%")
                            ),
                            column(5,
                              numericInput("corpus_type_max_articles",
                                           label = NULL,
                                           value = 60, min = 5, max = 500,
                                           step = 5, width = "100%")
                            )
                          )
                        )
                      )
                    ),
                    plotlyOutput("corpus_cite_type_plot", height = "440px")
                  ),
                  column(5,
                    p(class = "panel-hdr",
                      "SciScore per corpus"),
                    plotlyOutput("corpus_sci_bar_plot", height = "440px")
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                    p(class = "panel-hdr",
                      "Per-article SciScore"),
                    fluidRow(
                      column(4,
                        radioButtons("corpus_sci_view", label = NULL,
                                     choices = c("Box plot per corpus" = "box",
                                                 "Per-article bars"     = "bars"),
                                     selected = "box", inline = TRUE)
                      ),
                      column(4,
                        conditionalPanel(
                          "input.corpus_sci_view == 'bars'",
                          selectInput("corpus_sci_metric", label = NULL,
                                      choices = c("Journal %" = "journal_pct",
                                                  "DOI %"     = "doi_pct"),
                                      selected = "journal_pct",
                                      width = "100%")
                        )
                      ),
                      column(4,
                        conditionalPanel(
                          "input.corpus_sci_view == 'bars'",
                          numericInput("corpus_sci_max_articles",
                                       "Max articles to show",
                                       value = 60, min = 5, max = 500,
                                       step = 5, width = "100%")
                        )
                      )
                    ),
                    plotlyOutput("corpus_sci_article_plot", height = "440px")
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                    p(class = "panel-hdr", "Per-article SciScore table"),
                    if (has_DT) DT::DTOutput("corpus_sci_article_table")
                    else tableOutput("corpus_sci_article_table_base")
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                    p(class = "panel-hdr", "Citation table"),
                    if (has_DT) DT::DTOutput("corpus_cite_table")
                    else tableOutput("corpus_cite_table_base")
                  )
                ),
                br(),
                if (has_xlsx) fluidRow(
                  column(3,
                    downloadButton("dl_corpus_citations_xlsx",
                                   "⬇ Citations XLSX",
                                   class = "btn-sm btn-default",
                                   style = "width:100%;")
                  ),
                  column(3,
                    downloadButton("dl_corpus_summary_xlsx",
                                   "⬇ Per-corpus SciScore XLSX",
                                   class = "btn-sm btn-default",
                                   style = "width:100%;")
                  ),
                  column(3,
                    downloadButton("dl_corpus_sci_article_xlsx",
                                   "⬇ Per-article SciScore XLSX",
                                   class = "btn-sm btn-default",
                                   style = "width:100%;")
                  ),
                  column(3,
                    downloadButton("dl_corpus_sci_combined_xlsx",
                                   "⬇ All SciScore (multi-sheet)",
                                   class = "btn-sm btn-default",
                                   style = "width:100%;")
                  )
                )
              ),

              tabPanel("Co-citation Network",
                br(),
                if (has_vis)
                  visNetworkOutput("corpus_cocite", height = "600px")
                else
                  div(class = "latency-section",
                      p("Install the ", tags$code("visNetwork"),
                        " package for network plots."))
              ),
              tabPanel("Publication Network",
                br(),
                if (has_vis) tagList(
                  fluidRow(
                    column(3,
                      numericInput("pubnet_min_wiki",
                                   "Min articles citing each DOI",
                                   value = 2, min = 1, max = 50, step = 1,
                                   width = "100%")
                    ),
                    column(3,
                      numericInput("pubnet_top_n",
                                   "Top N most-cited DOIs",
                                   value = 50, min = 5, max = 500, step = 5,
                                   width = "100%")
                    )
                  ),
                  visNetworkOutput("corpus_pubnet", height = "600px")
                ) else
                  div(class = "latency-section",
                      p("Install the ", tags$code("visNetwork"),
                        " package for network plots."))
              ),
              tabPanel("Wikilink Network",
                br(),
                if (has_vis) tagList(
                  fluidRow(
                    column(3,
                      numericInput("wikilink_min_sources",
                                   "Min corpus articles linking to each target",
                                   value = 2, min = 1, max = 50, step = 1,
                                   width = "100%")
                    ),
                    column(3,
                      numericInput("wikilink_top_n",
                                   "Top N most-linked targets",
                                   value = 75, min = 5, max = 500, step = 5,
                                   width = "100%")
                    ),
                    column(3,
                      checkboxInput("wikilink_corpus_only",
                                    "Show only links between corpus articles",
                                    value = FALSE, width = "100%")
                    )
                  ),
                  visNetworkOutput("corpus_wikilink", height = "600px")
                ) else
                  div(class = "latency-section",
                      p("Install the ", tags$code("visNetwork"),
                        " package for network plots."))
              )
            )
          )
        )
      )
    )
  ),

  # ════════════════════════════════════════════════════════════════════════
  # HELP
  # ════════════════════════════════════════════════════════════════════════
  tabPanel(
    "Help",
    value = "help",
    help_panel
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # Hard cap to keep prepare_history() responsive even if user enters a
  # large sample size.
  N_HARD_CAP <- 10000L

  # ── Defensive helpers ─────────────────────────────────────────────────────
  # Run an expression and surface failures as a notification + safe fallback,
  # so a transient API hiccup or a bad input combination doesn't tear down
  # the Shiny session. Prefer this over letting tryCatch be optional in
  # observers (where unhandled errors otherwise crash the session).
  safely <- function(expr, fallback = NULL, label = "operation") {
    tryCatch(
      withCallingHandlers(
        expr,
        warning = function(w) invokeRestart("muffleWarning")
      ),
      error = function(e) {
        msg <- conditionMessage(e)
        if (!grepl("^(req\\(|need\\()", msg)) {
          showNotification(
            sprintf("%s failed: %s", label,
                    substr(msg, 1, 240)),
            type = "warning", duration = 6
          )
        }
        fallback
      }
    )
  }


  # ════════════════════════════════════════════════════════════════════════
  # SINGLE ARTICLE: fetch, sampling, history reactive
  # ════════════════════════════════════════════════════════════════════════

  raw_hist <- eventReactive(input$go, {
    article <- trimws(input$article)
    shiny::validate(shiny::need(nzchar(article), "Please enter an article name."))
    shiny::validate(shiny::need(nchar(article) <= 255, "Article name is too long."))
    shiny::validate(shiny::need(
      !grepl("[<>{}|\\[\\]\n\r\t]", article),
      "Article name contains invalid characters."
    ))

    cache_dir <- tryCatch(
      tools::R_user_dir("wikiliteApp", which = "cache"),
      error = function(e) file.path(tempdir(), "wikiliteApp_cache")
    )
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

    withProgress(message = paste0("Fetching '", article, "' …"), value = 0.2, {
      raw <- tryCatch(
        fetch_history(
          article,
          date_an   = format(as.POSIXct(input$date_to, tz = "UTC"),
                             "%Y-%m-%dT%H:%M:%SZ"),
          cache_dir = cache_dir
        ),
        error = function(e) {
          showNotification(paste("Fetch error:", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )
      req(!is.null(raw))

      # Lightweight pre-prep so date filtering + sampling can happen on the
      # raw frame WITHOUT first running the expensive prepare_history() over
      # all 100k+ revisions. We only parse timestamps and sort here.
      incProgress(0.4, detail = sprintf("Indexing %s revisions…",
                                        format(nrow(raw), big.mark = ",")))
      if ("ts" %in% names(raw) && !"timestamp" %in% names(raw))
        raw$timestamp <- raw$ts
      raw$ts_posix <- parse_ts(raw$timestamp)
      raw <- raw[order(raw$ts_posix), , drop = FALSE]
      attr(raw, "art") <- article

      n_raw <- nrow(raw)
      cap   <- min(N_HARD_CAP,
                   max(50L, as.integer(input$sample_n %||% 1500L)))
      if (n_raw > cap) {
        showNotification(
          sprintf("'%s' has %s revisions — sampling %s using '%s'.",
                  article,
                  format(n_raw, big.mark = ","),
                  format(cap, big.mark = ","),
                  input$sample_method %||% "recent"),
          type = "message", duration = 8
        )
      }
      raw
    })
  })

  # Debounce slow-to-rerun parameters so rapid spinner clicks don't trigger
  # a cascade of hist_data() rebuilds. 600 ms is fast enough to feel
  # responsive but coarse enough to absorb a flurry of clicks.
  date_from_d <- reactive(input$date_from)     %>% debounce(600)
  date_to_d   <- reactive(input$date_to)       %>% debounce(600)
  min_delta_d <- reactive(input$min_delta)     %>% debounce(600)
  n_top_d     <- reactive(input$n_top)         %>% debounce(600)
  sample_n_d  <- reactive(input$sample_n)      %>% debounce(600)
  sample_m_d  <- reactive(input$sample_method) %>% debounce(300)

  hist_data <- reactive({
    req(raw_hist())
    safely({
      raw <- raw_hist()

      d_from <- as.POSIXct(date_from_d(), tz = "UTC")
      d_to   <- as.POSIXct(date_to_d(),   tz = "UTC")
      if (is.na(d_from) || is.na(d_to) || d_to < d_from) {
        showNotification("Invalid date range.", type = "warning")
        req(FALSE)
      }
      raw <- raw[raw$ts_posix >= d_from & raw$ts_posix <= d_to, , drop = FALSE]

      n_target <- min(N_HARD_CAP,
                      max(50L, as.integer(sample_n_d() %||% 1500L)))
      method   <- sample_m_d() %||% "recent"
      raw <- sample_revisions(raw, n = n_target, method = method)

      if (nrow(raw) < 3) {
        showNotification("Too few revisions in the selected date range.",
                         type = "warning", duration = 6)
        req(FALSE)
      }

      n_top <- max(4L, min(15L, as.integer(n_top_d() %||% 8L)))
      h <- prepare_history(raw, n_top = n_top)
      if (!"art" %in% names(h)) {
        h$art <- attr(raw, "art") %||% trimws(input$article)
      }

      min_d <- as.numeric(min_delta_d() %||% 0)
      if (!is.na(min_d) && min_d > 0) {
        h <- h %>% filter(row_number() == 1L | abs(size_delta) >= min_d)
      }
      if (nrow(h) < 3) {
        showNotification("After Min |Δ| filter, fewer than 3 revisions remain.",
                         type = "warning", duration = 6)
        req(FALSE)
      }

      h %>% mutate(
        rev_index  = row_number(),
        size_delta = size - lag(size, default = first(size))
      )
    }, label = "History preparation")
  })

  own_data <- reactive({
    req(hist_data())
    n_top  <- max(4L, min(15L, as.integer(input$n_top)))
    h      <- subsample_history(hist_data(), max_n = 20)
    own_df <- build_content_ownership(h)
    own_c  <- collapse_minor_authors(own_df, n_top = n_top)
    list(h = h, own_df = own_df, own_c = own_c)
  })

  # ── Author list (left of History Flow) ──────────────────────────────────
  output$author_list <- renderUI({
    req(hist_data(), own_data())

    h   <- hist_data()
    od  <- own_data()
    cur_disabled <- isolate(user_enabled_rv())
    cur_colors   <- isolate(user_colors_rv())

    OTHER_GRPS <- c("Other", "Anonymous", "Other (registered)")

    grp_summary <- od$own_c %>%
      group_by(grp) %>%
      summarise(total_bytes = sum(bytes), .groups = "drop") %>%
      arrange(desc(total_bytes))

    base_pal <- author_palette(unique(grp_summary$grp))
    base_pal[["Other"]]             <- "#DDDDDD"
    base_pal[["Other (hidden)"]]    <- "#BBBBBB"

    grp_chips <- lapply(seq_len(nrow(grp_summary)), function(i) {
      grp   <- grp_summary$grp[i]
      bytes <- grp_summary$total_bytes[i]
      col   <- cur_colors[[grp]] %||% base_pal[[grp]] %||% "#CCCCCC"
      is_named <- !grp %in% OTHER_GRPS
      is_on    <- !grp %in% cur_disabled
      kB_str   <- if (bytes > 1000) sprintf("%.1f kB", bytes / 1000)
                  else sprintf("%d B", as.integer(bytes))
      lbl_js   <- jsonlite::toJSON(grp, auto_unbox = TRUE)

      toggle_el <- if (is_named) {
        tags$input(
          type     = "checkbox",
          checked  = if (is_on) NA else NULL,
          style    = "cursor:pointer; margin:0; flex-shrink:0;",
          onchange = sprintf(paste0(
            "var c=this; var chip=c.closest('.grp-chip');",
            " chip.classList.toggle('disabled',!c.checked);",
            " Shiny.setInputValue('toggle_author',{label:%s,checked:c.checked},{priority:'event'});"
          ), lbl_js)
        )
      } else {
        tags$span(style = "width:15px; display:inline-block; flex-shrink:0;")
      }

      color_el <- tags$input(
        type     = "color",
        class    = "color-pick",
        value    = col,
        title    = paste("Change colour for", grp),
        onchange = sprintf(
          "Shiny.setInputValue('color_author',{label:%s,color:this.value},{priority:'event'});",
          lbl_js
        )
      )

      tags$div(
        class = paste("grp-chip", if (!is_on) "disabled" else ""),
        toggle_el, color_el,
        tags$span(
          style = "flex:1; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
          grp
        ),
        tags$span(style = "color:#888; font-size:10px; white-space:nowrap;", kB_str)
      )
    })

    ed <- editor_table(h, n_top = 50)
    user_label_map <- h %>%
      distinct(user, author_label) %>%
      { setNames(.$author_label, .$user) }
    ed_pal <- author_palette(unique(h$author_label))
    for (.lbl in names(cur_colors)) {
      if (.lbl %in% names(ed_pal)) ed_pal[[.lbl]] <- cur_colors[[.lbl]]
    }

    chips <- lapply(seq_len(nrow(ed)), function(i) {
      u   <- ed$user[i]
      lbl <- user_label_map[[u]] %||% "Other (registered)"
      col <- ed_pal[[lbl]] %||% "#CCCCCC"
      n   <- ed$n_edits[i]
      typ <- ed$type[i]

      tags$div(
        class = "author-chip",
        title = paste0(u, " · ", n, " edits · ", typ),
        tags$div(class = "swatch", style = paste0("background:", col, ";")),
        tags$div(class = "aname", u),
        tags$div(class = "acount", n)
      )
    })

    tagList(
      tags$div(style = "font-size:10px; color:#888; margin-bottom:4px;",
               sprintf("%d unique editors", length(unique(h$user)))),
      p(class = "panel-hdr", style = "margin:0 0 2px;", "Flow chart groups"),
      tagList(grp_chips),
      tags$hr(style = "margin:6px 0;"),
      p(class = "panel-hdr", style = "margin:0 0 2px;", "All editors"),
      tagList(chips)
    )
  })

  # ── History Flow Sankey ─────────────────────────────────────────────────
  output$hf_plot <- renderEcharts4r({
    req(hist_data(), nrow(hist_data()) >= 3)

    od            <- own_data()
    disabled      <- user_enabled_rv()
    custom_colors <- user_colors_rv()

    if (length(disabled) > 0) {
      own_c <- od$own_c %>%
        mutate(grp = if_else(grp %in% disabled, "Other (hidden)", grp)) %>%
        group_by(rev_index, ts_posix, grp) %>%
        summarise(bytes = sum(bytes), .groups = "drop")
    } else {
      own_c <- od$own_c
    }
    own_c <- own_c %>% filter(bytes > 0)
    req(nrow(own_c) > 0)

    all_grps <- unique(own_c$grp)
    pal      <- author_palette(all_grps)
    if ("Other" %in% all_grps) pal[["Other"]] <- "#DDDDDD"
    pal[["Other (hidden)"]] <- "#BBBBBB"
    for (.lbl in names(custom_colors)) {
      if (nzchar(custom_colors[[.lbl]] %||% "")) pal[[.lbl]] <- custom_colors[[.lbl]]
    }

    h_samp   <- od$h
    hl_rev   <- sankey_hl_rev_rv()

    rev_revid <- setNames(as.character(h_samp$revid),
                          as.character(h_samp$rev_index))
    max_rev   <- max(own_c$rev_index)

    author_order <- own_c %>%
      group_by(grp) %>%
      summarise(first_rev = min(rev_index), .groups = "drop") %>%
      arrange(first_rev) %>%
      mutate(author_rank = row_number())

    canvas_h <- 480L

    own_c_pos <- own_c %>%
      left_join(author_order, by = "grp") %>%
      group_by(rev_index) %>%
      arrange(author_rank, .by_group = TRUE) %>%
      mutate(
        col_total  = sum(bytes),
        cum_before = cumsum(lag(bytes, default = 0)),
        y_center   = as.integer(round((cum_before + bytes / 2) / col_total * canvas_h))
      ) %>%
      ungroup()

    node_y <- setNames(
      own_c_pos$y_center,
      paste0(own_c_pos$grp, "###", own_c_pos$rev_index)
    )

    own_c2 <- own_c %>% mutate(node_name = paste0(grp, "###", rev_index))
    src_df <- own_c2 %>% rename(r_src = rev_index, b_src = bytes, src_node = node_name)
    tgt_df <- own_c2 %>% rename(r_tgt = rev_index, b_tgt = bytes, tgt_node = node_name) %>%
      mutate(r_src = r_tgt - 1L)

    links_df <- inner_join(src_df, tgt_df, by = c("grp", "r_src")) %>%
      filter(b_src > 0, b_tgt > 0) %>%
      transmute(
        source = src_node,
        target = tgt_node,
        value  = round(pmin(b_src, b_tgt) / 1000, 3)
      )
    req(nrow(links_df) > 0)

    art_title <- unique(hist_data()$art)[1] %||% trimws(input$article)
    n_revs    <- length(unique(own_c$rev_index))
    n_grps    <- length(unique(own_c$grp))

    chart <- links_df %>%
      e_charts() %>%
      e_sankey(source, target, value, layoutIterations = 0L,
               top = "70px", bottom = "10px", left = "10px", right = "160px",
               nodeWidth = 8L, nodeGap = 6L) %>%
      e_title(
        text    = art_title,
        subtext = paste0(n_revs, " snapshots · ", n_grps, " editor groups · ",
                         "y = text position (proxy)")
      ) %>%
      e_tooltip(
        trigger   = "item",
        formatter = htmlwidgets::JS("
          function(p) {
            var clean = function(s){ return String(s).replace(/###\\d+$/, ''); };
            if (p.dataType === 'edge') {
              return clean(p.data.source) + ' → ' + clean(p.data.target) +
                     '<br/>' + Number(p.data.value).toFixed(2) + ' kB';
            }
            return '<b>' + clean(p.name) + '</b><br/>' +
                   Number(p.value || 0).toFixed(2) + ' kB' +
                   '<br/><small style=\"color:#aaa\">click to load revision</small>';
          }
        ")
      ) %>%
      e_on("series", htmlwidgets::JS("
        function(params) {
          if (params.dataType !== 'node' || !params.data || !params.data.revid) return;
          Shiny.setInputValue('hf_click_revid', params.data.revid, {priority: 'event'});
        }
      "))

    chart$x$opts$series[[1]]$data <- lapply(
      chart$x$opts$series[[1]]$data,
      function(n) {
        rev_idx    <- as.integer(sub("^.*###", "", n$name))
        grp        <- sub("###\\d+$", "", n$name)
        col        <- pal[[grp]] %||% "#CCCCCC"
        revid      <- rev_revid[[as.character(rev_idx)]] %||% ""
        y_val      <- node_y[[n$name]] %||% 0L
        is_hl      <- !is.null(hl_rev) && rev_idx == hl_rev

        list(
          name      = n$name,
          y         = y_val,
          revid     = revid,
          itemStyle = list(
            color       = col,
            borderColor = if (is_hl) "#000000" else col,
            borderWidth = if (is_hl) 3L else 0L
          ),
          label = list(
            show      = (rev_idx == max_rev),
            formatter = htmlwidgets::JS(
              "function(p){ return p.name.replace(/###\\d+$/, ''); }"
            )
          )
        )
      }
    )

    chart$x$opts$series[[1]]$lineStyle <- list(color = "source", opacity = 0.35)

    chart
  })

  # Tick whenever we want to (re-)load wikitext for the currently selected
  # revision. Bumped when switching to the Revision Inspector with stale data.
  inspector_load_tick <- reactiveVal(0L)

  observeEvent(input$single_tabs, {
    if (!isTRUE(input$single_tabs == "Revision Inspector")) return()
    if (!is.null(wikitext_rv()) || is.null(selected_row_rv())) return()
    if (is.null(input$rev_selector) || !nzchar(input$rev_selector)) return()
    inspector_load_tick(isolate(inspector_load_tick()) + 1L)
  }, ignoreInit = TRUE)

  # Sankey node click → jump to that revision in the Inspector tab
  observeEvent(input$hf_click_revid, {
    req(hist_data(), nzchar(input$hf_click_revid))
    h         <- hist_data()
    revid_str <- as.character(round(as.numeric(input$hf_click_revid)))
    if (revid_str %in% as.character(h$revid)) {
      updateSelectInput(session, "rev_selector", selected = revid_str)
      updateTabsetPanel(session, "single_tabs", selected = "Revision Inspector")
    }
  }, ignoreInit = TRUE)

  # ── Reactive vals for revision/colour state ─────────────────────────────
  selected_row_rv    <- reactiveVal(NULL)
  wikitext_rv        <- reactiveVal(NULL)
  authorship_rv      <- reactiveVal(NULL)
  diff_rv            <- reactiveVal(NULL)
  sankey_hl_rev_rv   <- reactiveVal(NULL)
  user_enabled_rv    <- reactiveVal(character(0))
  user_colors_rv     <- reactiveVal(list())

  observeEvent(hist_data(), {
    selected_row_rv(NULL)
    wikitext_rv(NULL)
    authorship_rv(NULL)
    diff_rv(NULL)
    sankey_hl_rev_rv(NULL)
    user_enabled_rv(character(0))
    user_colors_rv(list())

    h <- hist_data()
    choices <- setNames(
      as.character(h$revid),
      paste0(format(h$ts_posix, "%Y-%m-%d %H:%M"), " · ", h$user)
    )
    updateSelectInput(session, "rev_selector",
                      choices  = choices,
                      selected = as.character(h$revid[nrow(h)]))
  }, ignoreInit = FALSE)

  observeEvent(input$toggle_author, {
    req(input$toggle_author)
    lbl <- input$toggle_author$label
    chk <- isTRUE(input$toggle_author$checked)
    cur <- user_enabled_rv()
    user_enabled_rv(if (chk) setdiff(cur, lbl) else union(cur, lbl))
  }, ignoreInit = TRUE)

  observeEvent(input$color_author, {
    req(input$color_author)
    lbl <- input$color_author$label
    col <- input$color_author$color
    if (nzchar(lbl) && nzchar(col)) {
      colors <- user_colors_rv()
      colors[[lbl]] <- col
      user_colors_rv(colors)
    }
  }, ignoreInit = TRUE)

  # Update the Sankey highlight (cheap) for any rev change, and stash the
  # selected row so the metadata pane updates immediately. The expensive
  # wikitext + diff fetch is deferred unless the Inspector tab is active.
  observeEvent(input$rev_selector, {
    safely({
      req(hist_data(), nzchar(input$rev_selector))
      h <- hist_data()
      row_idx <- which(as.character(h$revid) == input$rev_selector)
      if (length(row_idx) == 0) return()
      row <- h[row_idx[1], ]

      selected_row_rv(row)
      wikitext_rv(NULL)
      authorship_rv(NULL)
      diff_rv(NULL)

      if (!is.null(own_data())) {
        h_samp_q  <- own_data()$h
        exact_idx <- which(as.character(h_samp_q$revid) == input$rev_selector)
        if (length(exact_idx) > 0) {
          sankey_hl_rev_rv(h_samp_q$rev_index[exact_idx[1]])
        } else {
          diffs <- abs(as.numeric(difftime(h_samp_q$ts_posix, row$ts_posix, units = "secs")))
          sankey_hl_rev_rv(h_samp_q$rev_index[which.min(diffs)])
        }
      }

      if (isTRUE(input$single_tabs == "Revision Inspector")) {
        load_revision_details(input$rev_selector)
      }
    }, label = "Revision change")
  }, ignoreInit = TRUE)

  observeEvent(inspector_load_tick(), {
    safely({
      req(input$rev_selector, nzchar(input$rev_selector))
      if (!is.null(wikitext_rv())) return()
      load_revision_details(input$rev_selector)
    }, label = "Inspector load")
  }, ignoreInit = TRUE)

  load_revision_details <- function(revid) {
    h <- hist_data()
    row_idx <- which(as.character(h$revid) == revid)
    if (length(row_idx) == 0) return()
    row_idx <- row_idx[1]
    row     <- h[row_idx, ]

    req(own_data())

    user_label_map <- h %>%
      distinct(user, author_label) %>%
      { setNames(.$author_label, .$user) }
    pal_auth <- author_palette(unique(h$author_label))

    h_samp   <- own_data()$h
    h_before <- h_samp[h_samp$ts_posix <= row$ts_posix, , drop = FALSE]
    if (nrow(h_before) == 0) h_before <- h_samp[1, , drop = FALSE]
    idx       <- unique(round(seq(1, nrow(h_before), length.out = min(5L, nrow(h_before)))))
    snap_rows <- h_before[idx, ]

    revid_str <- as.character(row$revid)

    article <- attr(raw_hist(), "art") %||% trimws(input$article)

    withProgress(message = "Loading revision…", value = 0, {
      incProgress(0.20, detail = "wikitext")
      wt <- fetch_wikitext_for_revid(revid_str)
      wikitext_rv(wt)

      if (is.null(wt)) {
        authorship_rv(NULL)
        diff_rv(list(fetch_failed = TRUE, which = "current",
                     revid = revid_str))
        return()
      }

      # Token-level attribution via WikiWho — far more accurate than the
      # local line-diff approach. Fall back to the local approach if
      # WikiWho is unavailable for this revision.
      incProgress(0.30, detail = "WikiWho token attribution")
      tokens <- fetch_wikiwho_tokens(article, revid_str)
      if (!is.null(tokens) && nrow(tokens) > 0L) {
        authorship_rv(
          build_wikiwho_attribution(tokens, h, pal_auth, user_label_map)
        )
      } else {
        # Fallback: heuristic line diff against snap_rows.
        showNotification(
          "WikiWho unavailable for this revision — falling back to line diff.",
          type = "warning", duration = 4
        )
        authorship_rv(
          build_line_attribution(wt, snap_rows, pal_auth, user_label_map)
        )
      }

      if (row_idx > 1) {
        incProgress(0.25, detail = "fetching prev. revision")
        prev_revid <- as.character(h$revid[row_idx - 1])
        wt_prev    <- fetch_wikitext_for_revid(prev_revid)
        if (is.null(wt_prev)) {
          diff_rv(list(fetch_failed = TRUE, which = "previous",
                       revid = prev_revid))
        } else {
          incProgress(0.25, detail = "computing diff")
          diff_rv(compute_line_diff(wt_prev, wt))
        }
      } else {
        diff_rv(list(first_rev = TRUE))
      }
    })
  }

  # ── Revision metadata ───────────────────────────────────────────────────
  output$rev_meta <- renderUI({
    row <- selected_row_rv()
    if (!is.null(row)) {
      p(class = "rev-meta",
        sprintf("Rev %s · %s · %s · %+d bytes",
                row$revid,
                format(row$ts_posix, "%Y-%m-%d %H:%M"),
                row$user,
                row$size_delta))
    } else {
      p(class = "rev-meta", "Select a revision above to load its text.")
    }
  })

  # ── Wikitext panel (coloured by author) ─────────────────────────────────
  output$wikitext_panel <- renderUI({
    auth <- authorship_rv()
    wt   <- wikitext_rv()
    d    <- diff_rv()

    if (!is.null(auth)) {
      tbl_rows <- if (!is.null(auth$summary_df) && nrow(auth$summary_df) > 0) {
        sd <- auth$summary_df
        if ("n_tokens" %in% names(sd)) {
          # WikiWho schema
          total <- sum(sd$n_tokens, na.rm = TRUE)
          rows_html <- paste(vapply(seq_len(nrow(sd)), function(i) {
            sprintf(
              '<tr><td><span class="attr-swatch" style="background:%s;"></span>%s</td><td style="text-align:right;">%s</td><td style="text-align:right;">%.1f%%</td></tr>',
              htmlEscape(sd$color[i]),
              htmlEscape(sd$label[i]),
              format(sd$n_tokens[i], big.mark = ","),
              100 * sd$n_tokens[i] / max(total, 1L)
            )
          }, character(1L)), collapse = "")
          HTML(paste0(
            '<table class="attr-table">',
            '<thead><tr><th>Editor</th><th style="text-align:right;">Tokens</th><th style="text-align:right;">Share</th></tr></thead>',
            '<tbody>', rows_html, '</tbody></table>'
          ))
        } else {
          # Line-diff fallback schema
          rows_html <- paste(vapply(seq_len(nrow(sd)), function(i) {
            sprintf(
              '<tr><td><span class="attr-swatch" style="background:%s;"></span>%s</td><td>%s</td><td style="text-align:right;">%d</td></tr>',
              htmlEscape(sd$color[i]),
              htmlEscape(sd$label[i]),
              format(sd$first_ins[i], "%Y-%m-%d"),
              sd$n_lines[i]
            )
          }, character(1L)), collapse = "")
          HTML(paste0(
            '<table class="attr-table">',
            '<thead><tr><th>Editor</th><th>First inserted</th><th style="text-align:right;">Lines</th></tr></thead>',
            '<tbody>', rows_html, '</tbody></table>'
          ))
        }
      } else NULL

      info <- if (identical(auth$source, "wikiwho")) {
        sprintf("Token-level attribution via WikiWho · %s tokens · hover for details",
                format(auth$n_snaps, big.mark = ","))
      } else {
        sprintf("Line attribution (local diff) · %d revision samples · hover lines for details",
                auth$n_snaps)
      }
      tagList(
        tags$div(style = "font-size:10px; color:#777; margin-bottom:4px;",
                 info),
        div(class = "wikibox", HTML(auth$html)),
        tbl_rows
      )
    } else if (!is.null(wt)) {
      wt_esc <- htmlEscape(wt)
      wt_hl  <- gsub("(&lt;ref[^/].*?&lt;/ref&gt;|&lt;ref\\s[^/]*/?&gt;)",
                     "<mark>\\1</mark>", wt_esc, perl = TRUE)
      div(class = "wikibox", HTML(wt_hl))
    } else if (isTRUE(d$fetch_failed) && identical(d$which, "current")) {
      tags$span(style = "color:#b31d28; font-size:11px;",
                sprintf("Could not fetch wikitext for revision %s ",
                        d$revid %||% ""),
                tags$em("(MediaWiki rate-limited or revision deleted). Try again in a few seconds."))
    } else {
      tags$span(style = "color:#999; font-family:inherit; font-size:11px;",
                "Select a revision from the dropdown above.")
    }
  })

  # ── Diff panel ──────────────────────────────────────────────────────────
  output$diff_panel <- renderUI({
    d <- diff_rv()

    if (is.null(d)) {
      return(tags$span(style = "color:#999; font-size:11px;",
                       "Select a revision to see changes."))
    }
    if (isTRUE(d$first_rev)) {
      return(tags$p(style = "color:#999; font-size:11px;",
                    "First revision in dataset — no previous revision to compare."))
    }
    if (isTRUE(d$fetch_failed)) {
      return(tags$p(
        style = "color:#b31d28; font-size:11px;",
        sprintf("Could not fetch the %s revision (%s).",
                d$which %||% "previous",
                d$revid %||% "?"),
        tags$br(),
        tags$em("MediaWiki may be rate-limiting; try again shortly or pick another revision.")
      ))
    }

    lines_html <- character(0)

    removed_show <- head(d$removed, 60)
    for (l in removed_show) {
      lines_html <- c(lines_html, sprintf(
        '<span class="diff-removed">− %s</span>', htmlEscape(l)
      ))
    }
    if (d$n_removed > 60) {
      lines_html <- c(lines_html, sprintf(
        '<span class="diff-note">… %d more removed lines not shown</span>',
        d$n_removed - 60
      ))
    }

    if (d$n_removed > 0 && d$n_added > 0) {
      lines_html <- c(lines_html,
                      '<span class="diff-note" style="border-top:1px solid #ddd;display:block;margin:4px 0;"></span>')
    }

    added_show <- head(d$added, 60)
    for (l in added_show) {
      lines_html <- c(lines_html, sprintf(
        '<span class="diff-added">+ %s</span>', htmlEscape(l)
      ))
    }
    if (d$n_added > 60) {
      lines_html <- c(lines_html, sprintf(
        '<span class="diff-note">… %d more added lines not shown</span>',
        d$n_added - 60
      ))
    }

    if (d$n_removed == 0 && d$n_added == 0) {
      lines_html <- '<span class="diff-note">No line-level changes (whitespace or formatting only).</span>'
    }

    tagList(
      tags$div(style = "font-size:10px; color:#666; margin-bottom:4px;",
               sprintf("%d removed · %d added · %d unchanged lines",
                       d$n_removed, d$n_added, d$n_unchanged)),
      div(class = "diff-box", HTML(paste(lines_html, collapse = "\n")))
    )
  })

  # ── Citations tab ───────────────────────────────────────────────────────
  cite_data_rv <- reactiveVal(NULL)

  observeEvent(input$analyse_citations, { safely({
    req(hist_data(), nrow(hist_data()) >= 3)
    hist  <- hist_data()
    n_sn  <- min(max(3L, as.integer(input$cite_n_snapshots)), nrow(hist), 30L)
    lang  <- "en"

    snap_rows <- hist[unique(round(seq(1, nrow(hist), length.out = n_sn))), ]

    withProgress(message = "Fetching wikitext for citations…",
                 detail = sprintf("0 / %d", nrow(snap_rows)), {
      rows <- vector("list", nrow(snap_rows))
      for (i in seq_len(nrow(snap_rows))) {
        incProgress(1 / nrow(snap_rows),
                    detail = sprintf("%d / %d", i, nrow(snap_rows)))

        wt    <- fetch_wikitext_for_revid(snap_rows$revid[i], lang = lang)
        cites <- extract_cites(wt)

        if (length(cites) > 0) {
          rows[[i]] <- tibble(
            rev_index = snap_rows$rev_index[i],
            ts_posix  = snap_rows$ts_posix[i],
            cite_raw  = cites
          )
        }
        Sys.sleep(0.05)
      }
    })

    mat_raw <- bind_rows(rows)

    if (nrow(mat_raw) == 0) {
      showNotification("No <ref>…</ref> citations found in sampled revisions.",
                       type = "warning")
      return()
    }

    last_snap_rev <- max(snap_rows$rev_index)

    mat <- mat_raw %>%
      group_by(cite_raw) %>%
      summarise(
        first_rev   = min(rev_index),
        last_rev    = max(rev_index),
        first_ts    = min(ts_posix),
        last_ts     = max(ts_posix),
        n_snapshots = n(),
        .groups = "drop"
      ) %>%
      mutate(
        still_present = last_rev >= last_snap_rev,
        label         = vapply(cite_raw, cite_label, character(1L)),
        type          = vapply(cite_raw, cite_type,  character(1L))
      ) %>%
      arrange(first_ts)

    cite_data_rv(list(mat = mat, snap_rows = snap_rows))
    showNotification(sprintf("Found %d unique citations.", nrow(mat)), type = "message")
  }, label = "Citation analysis") })

  # Shared palette for both timeline and the type-counts bar plot.
  type_pal <- c(
    "Journal"="#377EB8","Book"="#4DAF4A","Web"="#FF7F00",
    "News/Magazine"="#984EA3","Preprint"="#E41A1C","Thesis"="#F781BF",
    "Conference"="#A65628","Report"="#F4A460","Multimedia"="#FF69B4",
    "Legal/Patent"="#808080","Social Media"="#1DA1F2","DOI"="#00BCD4",
    "Other"="#AAAAAA"
  )

  # Single source of truth for citation filters. The timeline, the counts bar,
  # the table, and the per-type export all read this so they always agree.
  filtered_cite_rv <- reactive({
    cd <- cite_data_rv()
    req(cd, nrow(cd$mat) > 0)
    mat <- cd$mat

    # `Additions only` is relative to the FIRST sampled snapshot in the run,
    # not to the currently-filtered subset — use the unfiltered min.
    v1_rev  <- min(cd$mat$first_rev)
    display <- input$cite_display
    type_f  <- input$cite_type_filter

    if (identical(display, "Active in latest revision"))  mat <- filter(mat, still_present)
    if (identical(display, "Additions only (not in v1)")) mat <- filter(mat, first_rev > v1_rev)
    if (identical(display, "Removed only"))               mat <- filter(mat, !still_present)
    if (!is.null(type_f) && type_f != "All types")        mat <- filter(mat, type == type_f)
    mat
  })

  output$cite_plot <- renderPlotly({
    cd  <- cite_data_rv()
    req(cd)
    mat <- filtered_cite_rv()
    req(nrow(mat) > 0)

    display <- input$cite_display
    type_f  <- input$cite_type_filter

    mat <- mat %>%
      mutate(status  = if_else(still_present, "Active", "Removed"),
             y_order = row_number())

    art_title <- unique(hist_data()$art)[1] %||% trimws(input$article)

    fig <- plot_ly(source = "cite_source")
    types_present  <- unique(mat$type)
    seen_in_legend <- character(0)

    for (tp in types_present) {
      col <- type_pal[[tp]] %||% "#AAAAAA"
      for (st in c("Active", "Removed")) {
        d <- filter(mat, type == tp, status == st)
        if (nrow(d) == 0) next
        show_leg <- !(tp %in% seen_in_legend)
        if (show_leg) seen_in_legend <- c(seen_in_legend, tp)

        fig <- fig %>%
          add_segments(
            data        = d,
            x           = ~first_ts, xend = ~last_ts,
            y           = ~y_order,  yend = ~y_order,
            line        = list(width = if (st == "Active") 5 else 3, color = col),
            opacity     = if (st == "Active") 1.0 else 0.4,
            name        = tp,
            legendgroup = tp,
            showlegend  = show_leg,
            text        = ~paste0(
              "<b>", label, "</b><br>",
              "Type: ", type, "<br>",
              "Status: ", status, "<br>",
              "Added: ", format(first_ts, "%Y-%m-%d"), "<br>",
              "Last seen: ", format(last_ts, "%Y-%m-%d"), "<br>",
              "Snapshots: ", n_snapshots
            ),
            hoverinfo = "text"
          ) %>%
          add_markers(
            data        = d,
            x           = ~first_ts,
            y           = ~y_order,
            marker      = list(color = col, size = 7,
                               opacity = if (st == "Active") 1.0 else 0.4),
            name        = tp,
            legendgroup = tp,
            showlegend  = FALSE,
            text        = ~paste0(format(first_ts, "%Y-%m-%d"), " · ", type),
            hoverinfo   = "text"
          )
      }
    }

    fig %>%
      layout(
        title = list(
          text = paste0("<b>Citation Timeline — ", art_title, "</b><br>",
                        "<sup>", nrow(mat), " citations · ",
                        nrow(cd$snap_rows), " snapshots · ",
                        display, if (type_f != "All types") paste0(" · ", type_f) else "",
                        "</sup>"),
          font = list(size = 13)
        ),
        xaxis = list(title = "Date", type = "date"),
        yaxis = list(
          title      = "",
          tickvals   = mat$y_order,
          ticktext   = mat$label,
          tickfont   = list(size = 9),
          autorange  = "reversed"
        ),
        height = max(400L, nrow(mat) * 26L + 110L),
        margin = list(l = 260, b = 60),
        legend = list(orientation = "h", x = 0, y = -0.08,
                      title = list(text = "Reference type"))
      )
  })

  # ── Citation type count bar plot ────────────────────────────────────────
  output$cite_type_count_plot <- renderPlotly({
    mat <- filtered_cite_rv()
    req(nrow(mat) > 0)

    counts <- mat %>%
      mutate(status = if_else(still_present, "Active", "Removed")) %>%
      group_by(type, status) %>%
      summarise(n = dplyr::n(), .groups = "drop")

    type_order <- counts %>%
      group_by(type) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      arrange(desc(total)) %>%
      pull(type)

    counts$type <- factor(counts$type, levels = rev(type_order))

    type_cols <- vapply(type_order,
                        function(tp) type_pal[[tp]] %||% "#AAAAAA",
                        character(1L))
    names(type_cols) <- type_order

    plot_ly(
      data = counts,
      y = ~type,
      x = ~n,
      type = "bar",
      orientation = "h",
      color = ~status,
      colors = c("Active" = "#2c3e50", "Removed" = "#c0c0c0"),
      text = ~n,
      textposition = "outside",
      hovertemplate = paste0("<b>%{y}</b><br>%{x} citations · %{fullData.name}",
                             "<extra></extra>")
    ) %>%
      layout(
        barmode = "stack",
        title = list(text = sprintf("<b>%d citations · by type</b>",
                                    nrow(mat)),
                     font = list(size = 12)),
        xaxis = list(title = "Count"),
        yaxis = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 110, r = 30, t = 40, b = 40),
        legend = list(orientation = "h", x = 0, y = -0.12)
      )
  })

  # ── Wikipedia article hyperlink above tabs ──────────────────────────────
  output$wiki_link_ui <- renderUI({
    req(raw_hist())
    art <- attr(raw_hist(), "art") %||% trimws(input$article)
    if (is.null(art) || !nzchar(art)) return(NULL)
    wiki_url <- paste0("https://en.wikipedia.org/wiki/",
                       utils::URLencode(gsub(" ", "_", art), reserved = FALSE))
    div(class = "wiki-art-link",
        tags$a(href = wiki_url, target = "_blank", rel = "noopener noreferrer",
               paste0(art, " ↗ Wikipedia")))
  })

  # ── Citation table ──────────────────────────────────────────────────────
  if (has_DT) {
    output$cite_table <- DT::renderDT({
      mat  <- filtered_cite_rv()
      epmc <- epmc_rv()
      isbn <- isbn_rv()
      req(nrow(mat) > 0)
      df <- mat %>%
        mutate(
          .url  = vapply(cite_raw, function(x) cite_url(x) %||% "", character(1L)),
          .doi  = vapply(cite_raw, function(x) {
            m <- regmatches(x, regexpr("10\\.\\d{4,}/[-._;()/:a-zA-Z0-9]+", x, perl = TRUE))
            if (length(m) && nzchar(m)) m[[1]] else NA_character_
          }, character(1L)),
          .isbn = vapply(cite_raw, extract_isbn, character(1L)),
          Citation  = mapply(make_cite_link, label, .url),
          Type      = type,
          Added     = format(first_ts, "%Y-%m-%d"),
          `Last seen` = format(last_ts, "%Y-%m-%d"),
          Status    = if_else(still_present, "Active ✓", "Removed"),
          Snapshots = n_snapshots
        )
      have_epmc <- !is.null(epmc) && nrow(epmc) > 0
      have_isbn <- !is.null(isbn) && nrow(isbn) > 0
      if (have_epmc) {
        idx <- match(df$.doi, epmc$doi)
        df$Journal   <- epmc$journalTitle[idx]
        df$Year      <- epmc$pubYear[idx]
        df$OA        <- ifelse(!is.na(idx) & epmc$isOpenAccess[idx] == "Y", "OA ✓", "")
        df$Citations <- epmc$citedByCount[idx]
      }
      if (have_isbn) {
        idx2 <- match(df$.isbn, isbn$isbn)
        df$`Book title` <- ifelse(is.na(idx2), NA_character_, isbn$title[idx2])
        df$Publisher    <- ifelse(is.na(idx2), NA_character_, isbn$publisher[idx2])
        df$Authors      <- ifelse(is.na(idx2), NA_character_, isbn$authors[idx2])
      }
      cols <- c("Citation", "Type")
      if (have_epmc) cols <- c(cols, "Journal", "Year", "OA", "Citations")
      if (have_isbn) cols <- c(cols, "Book title", "Publisher", "Authors")
      cols <- c(cols, "Added", "Last seen", "Status", "Snapshots")
      df <- df[, intersect(cols, names(df)), drop = FALSE]
      df
    },
    escape   = FALSE,
    options  = list(pageLength = 10, scrollX = TRUE, dom = "ftp"),
    filter   = "top",
    rownames = FALSE,
    class    = "compact stripe hover"
    )
  } else {
    output$cite_table_base <- renderTable({
      mat <- filtered_cite_rv()
      req(nrow(mat) > 0)
      mat %>%
        mutate(
          Added       = format(first_ts, "%Y-%m-%d"),
          `Last seen` = format(last_ts,  "%Y-%m-%d"),
          Status      = if_else(still_present, "Active", "Removed")
        ) %>%
        select(label, type, Added, `Last seen`, Status, n_snapshots) %>%
        rename(Citation = label, Type = type, Snapshots = n_snapshots)
    })
  }

  # Helper: build the export data frame for a given subset, including any
  # EuropePMC / Google Books annotations available in the session.
  build_cite_export_df <- function(subset_mat) {
    if (is.null(subset_mat) || nrow(subset_mat) == 0L) return(NULL)
    epmc <- epmc_rv()
    isbn <- isbn_rv()
    df <- subset_mat %>%
      mutate(
        URL       = vapply(cite_raw, function(x) cite_url(x) %||% "", character(1L)),
        DOI       = vapply(cite_raw, function(x) {
          m <- regmatches(x, regexpr("10\\.\\d{4,}/[-._;()/:a-zA-Z0-9]+", x, perl = TRUE))
          if (length(m) && nzchar(m)) m[[1]] else NA_character_
        }, character(1L)),
        ISBN      = vapply(cite_raw, extract_isbn, character(1L)),
        Added     = format(first_ts, "%Y-%m-%d"),
        `Last seen` = format(last_ts, "%Y-%m-%d"),
        Status    = if_else(still_present, "Active", "Removed")
      ) %>%
      rename(Citation = label, Type = type, Snapshots = n_snapshots,
             `Raw wikitext` = cite_raw)
    if (!is.null(epmc) && nrow(epmc) > 0) {
      idx <- match(df$DOI, epmc$doi)
      df$Journal   <- epmc$journalTitle[idx]
      df$Year      <- epmc$pubYear[idx]
      df$OA        <- ifelse(!is.na(idx) & epmc$isOpenAccess[idx] == "Y", "OA", "")
      df$Citations <- epmc$citedByCount[idx]
    }
    if (!is.null(isbn) && nrow(isbn) > 0) {
      idx2 <- match(df$ISBN, isbn$isbn)
      df$`Book title` <- ifelse(is.na(idx2), NA_character_, isbn$title[idx2])
      df$Publisher    <- ifelse(is.na(idx2), NA_character_, isbn$publisher[idx2])
      df$Authors      <- ifelse(is.na(idx2), NA_character_, isbn$authors[idx2])
    }
    df %>%
      select(any_of(c("Citation", "Type",
                      "Journal", "Year", "OA", "Citations",
                      "Book title", "Publisher", "Authors",
                      "Added", "Last seen", "Status", "Snapshots",
                      "DOI", "ISBN", "URL", "Raw wikitext")))
  }

  if (has_xlsx) {
    # Master XLSX: all citations (unfiltered) with all annotations.
    output$dl_cite_xlsx <- downloadHandler(
      filename = function() {
        art <- gsub("[^A-Za-z0-9_-]", "_", trimws(input$article))
        paste0(art, "_citations.xlsx")
      },
      content = function(file) {
        cd <- cite_data_rv()
        req(cd)
        openxlsx::write.xlsx(build_cite_export_df(cd$mat), file)
      }
    )

    # Per-type or filtered-only XLSX: respects the Reference type selector
    # in the export bar (or "All filtered" → respects status + type filters).
    output$dl_cite_by_type <- downloadHandler(
      filename = function() {
        art   <- gsub("[^A-Za-z0-9_-]", "_", trimws(input$article))
        scope <- input$export_type %||% "All"
        slug  <- gsub("[^A-Za-z0-9]+", "_", scope)
        paste0(art, "_citations_", slug, ".xlsx")
      },
      content = function(file) {
        cd <- cite_data_rv()
        req(cd)
        scope <- input$export_type %||% "All filtered"
        sub_mat <- if (identical(scope, "All filtered")) {
          filtered_cite_rv()
        } else {
          cd$mat %>% filter(type == scope)
        }
        if (is.null(sub_mat) || nrow(sub_mat) == 0L) {
          showNotification("No citations match this export selection.",
                           type = "warning")
          # Write an empty workbook so the download completes cleanly.
          openxlsx::write.xlsx(data.frame(Message = "No citations matched."), file)
          return()
        }
        openxlsx::write.xlsx(build_cite_export_df(sub_mat), file)
      }
    )
  }

  # ── Authorship tab ──────────────────────────────────────────────────────
  auth_stats_rv <- reactive({ req(hist_data()); authorship_stats(hist_data()) })
  auth_editors_rv <- reactive({ req(hist_data()); editor_table(hist_data(), n_top = 100L) })

  output$auth_cards <- renderUI({
    d <- auth_stats_rv()
    req(d)
    metric <- function(val, lbl)
      tags$div(class = "metric-card",
               tags$div(class = "metric-val", val),
               tags$div(class = "metric-lbl", lbl))
    div(class = "metric-row",
        metric(format(d$n_revisions, big.mark = ","),     "Revisions"),
        metric(format(d$n_unique_editors, big.mark = ","),"Editors"),
        metric(sprintf("%.1f%%", d$pct_anonymous),        "Anonymous"),
        metric(sprintf("%.2f",   d$gini_editors %||% NA), "Gini"),
        metric(d$top_editor,                              "Top editor"),
        metric(sprintf("%.1f%%", d$top_editor_pct),       "Top editor share")
    )
  })

  output$auth_anon_plot <- renderPlot({ req(hist_data()); print(plot_anon_vs_registered(hist_data())) }, res = 96)
  output$auth_top_plot  <- renderPlot({ req(hist_data()); print(plot_top_editors(hist_data())) }, res = 96)
  output$auth_cum_plot  <- renderPlotly({ req(hist_data()); plot_cumulative_edits(hist_data()) })

  if (has_DT) {
    output$editor_tbl <- DT::renderDT({
      ed <- auth_editors_rv()
      req(ed)
      ed %>%
        mutate(
          user_link = paste0(
            '<a href="https://en.wikipedia.org/wiki/User:',
            utils::URLencode(user, reserved = FALSE),
            '" target="_blank" rel="noopener noreferrer">', htmltools::htmlEscape(user), ' ↗</a>'
          ),
          `Revert rate` = sprintf("%.1f%%", revert_rate * 100),
          `Avg Δ bytes` = round(mean_size_delta),
          `First edit`  = format(as.POSIXct(first_edit, tz = "UTC"), "%Y-%m-%d"),
          `Last edit`   = format(as.POSIXct(last_edit,  tz = "UTC"), "%Y-%m-%d")
        ) %>%
        select(user_link, type, n_edits, n_reverts, `Revert rate`, `Avg Δ bytes`,
               `First edit`, `Last edit`) %>%
        rename(User = user_link, Type = type, Edits = n_edits, Reverts = n_reverts)
    },
    escape = FALSE,
    options = list(pageLength = 15, scrollX = TRUE, dom = "ftp"),
    filter  = "top", rownames = FALSE, class = "compact stripe hover")
  } else {
    output$editor_tbl_base <- renderTable({ auth_editors_rv() })
  }

  if (has_xlsx) {
    output$dl_editors_xlsx <- downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_-]", "_", trimws(input$article)), "_editors.xlsx"),
      content  = function(file) {
        req(auth_editors_rv())
        openxlsx::write.xlsx(as.data.frame(auth_editors_rv()), file)
      }
    )
  }

  # ── Stability tab ───────────────────────────────────────────────────────
  stab_stats_rv <- reactive({ req(hist_data()); stability_stats(hist_data()) })

  output$stab_cards <- renderUI({
    d <- stab_stats_rv()
    req(d)
    metric <- function(val, lbl)
      tags$div(class = "metric-card",
               tags$div(class = "metric-val", val),
               tags$div(class = "metric-lbl", lbl))
    div(class = "metric-row",
        metric(sprintf("%.1f kB", d$final_size_kB),   "Current size"),
        metric(sprintf("%.1f kB", d$peak_size_kB),    "Peak size"),
        metric(sprintf("%.0f%%",  d$size_growth_pct), "Growth vs v1"),
        metric(sprintf("%.2f",    d$size_cv),         "Size CV"),
        metric(sprintf("%.0f%%",  d$pct_edits_shrink),"Shrinking edits"),
        metric(sprintf("%.0f%%",  d$pct_edits_grow),  "Growing edits")
    )
  })

  output$stab_size_plot   <- renderPlotly({ req(hist_data()); plot_size_timeline(hist_data(), interactive = TRUE) })
  output$stab_delta_plot  <- renderPlot({ req(hist_data()); print(plot_delta_distribution(hist_data())) }, res = 96)
  output$stab_rhythm_plot <- renderPlot({ req(hist_data()); print(plot_editing_rhythm(hist_data())) }, res = 96)

  if (has_xlsx) {
    output$dl_history_xlsx <- downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_-]", "_", trimws(input$article)), "_revisions.xlsx"),
      content  = function(file) {
        req(hist_data())
        df <- hist_data() %>%
          select(rev_index, revid, ts_posix, user, is_anon, author_label,
                 size, size_delta, is_rev, comment) %>%
          mutate(ts_posix = format(ts_posix, "%Y-%m-%d %H:%M:%S"))
        openxlsx::write.xlsx(as.data.frame(df), file)
      }
    )
  }

  # ── SciScore over time (in Stability tab) ───────────────────────────────
  # For each sampled citation snapshot, count how many citations are
  # "active" at that snapshot (i.e. first_rev <= snap <= last_rev) and what
  # fraction are journals / DOI-linked. Returns NULL when the user hasn't
  # run the Citations tab yet.
  sci_timeline_rv <- reactive({
    cd <- cite_data_rv()
    if (is.null(cd) || is.null(cd$mat) || nrow(cd$mat) == 0L) return(NULL)
    if (is.null(cd$snap_rows) || nrow(cd$snap_rows) == 0L) return(NULL)

    mat <- cd$mat
    has_doi <- vapply(mat$cite_raw, function(x)
      grepl("10\\.\\d{4,}/", x, perl = TRUE), logical(1L))

    snaps <- cd$snap_rows[order(cd$snap_rows$rev_index), ,drop = FALSE]
    do.call(rbind, lapply(seq_len(nrow(snaps)), function(i) {
      r  <- snaps$rev_index[i]
      ts <- snaps$ts_posix[i]
      active <- mat$first_rev <= r & mat$last_rev >= r
      n_tot  <- sum(active)
      if (n_tot == 0L) {
        data.frame(rev_index = r, ts_posix = ts,
                   n_total = 0L, n_journal = 0L, n_doi = 0L,
                   sci_journal_pct = NA_real_, sci_doi_pct = NA_real_,
                   stringsAsFactors = FALSE)
      } else {
        n_jr <- sum(active & mat$type == "Journal", na.rm = TRUE)
        n_di <- sum(active & has_doi, na.rm = TRUE)
        data.frame(rev_index = r, ts_posix = ts,
                   n_total = n_tot, n_journal = n_jr, n_doi = n_di,
                   sci_journal_pct = round(100 * n_jr / n_tot, 1),
                   sci_doi_pct     = round(100 * n_di / n_tot, 1),
                   stringsAsFactors = FALSE)
      }
    }))
  })

  output$stab_sci_cards <- renderUI({
    df <- sci_timeline_rv()
    if (is.null(df) || nrow(df) == 0L) {
      return(div(class = "metric-row",
        tags$div(class = "metric-card",
          style = "min-width:280px;",
          tags$div(class = "metric-val", "—"),
          tags$div(class = "metric-lbl",
                   "Run the Citations tab to populate"))
      ))
    }

    valid <- df[!is.na(df$sci_journal_pct), , drop = FALSE]
    metric <- function(val, lbl)
      tags$div(class = "metric-card",
               tags$div(class = "metric-val", val),
               tags$div(class = "metric-lbl", lbl))

    if (nrow(valid) == 0L) {
      return(div(class = "metric-row",
        metric("0", "Snapshots with citations")
      ))
    }

    # Trend = first → last per-snapshot value (in percentage points).
    delta_jr <- valid$sci_journal_pct[nrow(valid)] - valid$sci_journal_pct[1]
    delta_di <- valid$sci_doi_pct[nrow(valid)]     - valid$sci_doi_pct[1]
    fmt_delta <- function(d)
      if (is.na(d)) "—" else sprintf("%+0.1f pp", d)

    div(class = "metric-row",
      metric(sprintf("%.1f%%", valid$sci_journal_pct[nrow(valid)]), "Latest journal %"),
      metric(sprintf("%.1f%%", valid$sci_doi_pct[nrow(valid)]),     "Latest DOI %"),
      metric(fmt_delta(delta_jr), "Δ journal % (v1 → latest)"),
      metric(fmt_delta(delta_di), "Δ DOI % (v1 → latest)"),
      metric(format(max(valid$n_total), big.mark = ","), "Peak active citations")
    )
  })

  output$stab_sci_timeline_plot <- renderPlotly({
    df <- sci_timeline_rv()
    if (is.null(df) || nrow(df) == 0L) return(plotly::plotly_empty())

    art_title <- unique(hist_data()$art)[1] %||% trimws(input$article)
    fig <- plot_ly() %>%
      add_lines(x = df$ts_posix, y = df$sci_journal_pct,
                name = "Journal %", line = list(color = "#377EB8", width = 3),
                marker = list(color = "#377EB8"),
                hovertemplate = paste0(
                  "<b>%{x|%Y-%m-%d}</b><br>",
                  "Journal: %{y:.1f}%<extra></extra>")) %>%
      add_markers(x = df$ts_posix, y = df$sci_journal_pct,
                  marker = list(color = "#377EB8", size = 7),
                  showlegend = FALSE,
                  hoverinfo = "skip") %>%
      add_lines(x = df$ts_posix, y = df$sci_doi_pct,
                name = "DOI %", line = list(color = "#4DAF4A", width = 3),
                marker = list(color = "#4DAF4A"),
                hovertemplate = paste0(
                  "<b>%{x|%Y-%m-%d}</b><br>",
                  "DOI: %{y:.1f}%<extra></extra>")) %>%
      add_markers(x = df$ts_posix, y = df$sci_doi_pct,
                  marker = list(color = "#4DAF4A", size = 7),
                  showlegend = FALSE,
                  hoverinfo = "skip") %>%
      layout(
        title = list(text = sprintf("<b>SciScore over time — %s</b>",
                                    art_title),
                     font = list(size = 13)),
        xaxis = list(title = "Snapshot date"),
        yaxis = list(title = "% of active citations",
                     range = c(0, 100),
                     ticksuffix = "%"),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = -0.18)
      )
    fig
  })

  output$stab_sci_type_mix_plot_ui <- renderUI({
    df <- sci_timeline_rv()
    if (is.null(df) || nrow(df) == 0L) return(NULL)
    plotlyOutput("stab_sci_type_mix_plot", height = "260px")
  })

  output$stab_sci_type_mix_plot <- renderPlotly({
    cd <- cite_data_rv()
    req(cd, !is.null(cd$snap_rows), nrow(cd$snap_rows) > 0)
    mat <- cd$mat

    snaps <- cd$snap_rows[order(cd$snap_rows$rev_index), , drop = FALSE]
    rows <- do.call(rbind, lapply(seq_len(nrow(snaps)), function(i) {
      r  <- snaps$rev_index[i]
      ts <- snaps$ts_posix[i]
      active <- mat$first_rev <= r & mat$last_rev >= r
      sub <- mat[active, , drop = FALSE]
      if (nrow(sub) == 0L) return(NULL)
      tab <- as.data.frame(table(sub$type), stringsAsFactors = FALSE)
      names(tab) <- c("type", "n")
      tab$ts_posix <- ts
      tab
    }))
    req(!is.null(rows), nrow(rows) > 0)

    rows$type <- factor(rows$type, levels = names(type_pal))

    plot_ly(rows, x = ~ts_posix, y = ~n, color = ~type,
            colors = type_pal, type = "bar",
            hovertemplate = paste0(
              "%{fullData.name}: %{y}<br>",
              "Snapshot: %{x|%Y-%m-%d}<extra></extra>")) %>%
      layout(
        title = list(text = "<b>Citation type mix per snapshot</b>",
                     font = list(size = 12)),
        xaxis = list(title = "Snapshot date"),
        yaxis = list(title = "Active citations"),
        barmode = "stack",
        legend = list(orientation = "h", x = 0, y = -0.18)
      )
  })

  if (has_xlsx) {
    output$dl_sci_timeline_xlsx <- downloadHandler(
      filename = function() paste0(
        gsub("[^A-Za-z0-9_-]", "_", trimws(input$article)),
        "_sciscore_timeline.xlsx"
      ),
      content = function(file) {
        df <- sci_timeline_rv()
        if (is.null(df) || nrow(df) == 0L) {
          openxlsx::write.xlsx(
            data.frame(Message = "Run Citations tab first to populate SciScore."),
            file
          )
          return()
        }
        out <- df
        out$ts_posix <- format(out$ts_posix, "%Y-%m-%d %H:%M:%S")
        openxlsx::write.xlsx(out, file)
      }
    )
  }

  # ── Vandalism & Edit Wars tab ───────────────────────────────────────────
  vandalism_rv <- reactive({
    req(hist_data())
    h <- detect_vandalism(hist_data())
    s <- compute_survival(h)
    list(hist = h, survival = s)
  })

  edit_war_rv <- reactive({ req(hist_data()); detect_edit_wars(hist_data()) })

  output$van_cards <- renderUI({
    vd   <- vandalism_rv()
    wars <- edit_war_rv()
    req(vd)
    n_van  <- nrow(vd$survival)
    n_rep  <- if (n_van > 0) sum(vd$survival$was_repaired, na.rm = TRUE) else 0L
    med_t  <- if (n_van > 0 && n_rep > 0)
                sprintf("%.0f min", median(vd$survival$survival_min, na.rm = TRUE))
              else "—"
    metric <- function(val, lbl)
      tags$div(class = "metric-card",
               tags$div(class = "metric-val", val),
               tags$div(class = "metric-lbl", lbl))
    div(class = "metric-row",
        metric(n_van,          "Vandalism events"),
        metric(n_rep,          "Repaired"),
        metric(med_t,          "Median repair time"),
        metric(nrow(wars),     "Edit war episodes"),
        metric(sum(wars$n_reverts, na.rm = TRUE), "Total reverts in wars")
    )
  })

  output$van_timeline_plot <- renderPlotly({
    vd <- vandalism_rv()
    req(vd)
    plot_vandalism_timeline(vd$hist, vd$survival, interactive = TRUE)
  })

  output$van_survival_plot <- renderPlot({
    s <- vandalism_rv()$survival
    if (is.null(s) || nrow(s) == 0) {
      plot.new(); title("No vandalism events detected")
      return(invisible(NULL))
    }
    print(plot_survival_dist(s))
  }, res = 96)

  output$war_plot <- renderPlotly({
    h    <- vandalism_rv()$hist
    wars <- edit_war_rv()
    req(h, wars)
    plot_edit_wars(h, wars, interactive = TRUE)
  })

  if (has_DT) {
    output$war_tbl <- DT::renderDT({
      wars <- edit_war_rv()
      req(wars)
      if (nrow(wars) == 0) return(data.frame(Message = "No edit war episodes detected."))
      wars %>%
        mutate(
          start_ts   = format(start_ts,   "%Y-%m-%d"),
          end_ts     = format(end_ts,     "%Y-%m-%d"),
          duration_h = round(duration_h, 1)
        ) %>%
        select(start_ts, end_ts, duration_h, n_reverts, n_editors, editors) %>%
        rename(`Start` = start_ts, `End` = end_ts,
               `Duration (h)` = duration_h, Reverts = n_reverts,
               `# Editors` = n_editors, Editors = editors)
    },
    options = list(pageLength = 10, scrollX = TRUE, dom = "ftp"),
    rownames = FALSE, class = "compact stripe hover")
  } else {
    output$war_tbl_base <- renderTable({ edit_war_rv() })
  }

  if (has_xlsx) {
    output$dl_vandalism_xlsx <- downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_-]", "_", trimws(input$article)), "_vandalism.xlsx"),
      content  = function(file) {
        s <- vandalism_rv()$survival
        req(s)
        openxlsx::write.xlsx(as.data.frame(s), file)
      }
    )
    output$dl_wars_xlsx <- downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_-]", "_", trimws(input$article)), "_edit_wars.xlsx"),
      content  = function(file) {
        wars <- edit_war_rv()
        req(wars)
        openxlsx::write.xlsx(as.data.frame(wars), file)
      }
    )
  }

  # ── SciScore gauge cards ────────────────────────────────────────────────
  sci_scores_rv <- reactive({
    cd <- cite_data_rv()
    req(!is.null(cd), !is.null(cd$mat), nrow(cd$mat) > 0)
    active <- cd$mat[cd$mat$still_present, ]
    n_tot  <- nrow(active)
    n_jour <- sum(active$type == "Journal", na.rm = TRUE)
    n_doi  <- sum(vapply(active$cite_raw, function(x)
      grepl("10\\.\\d{4,}/", x, perl = TRUE), logical(1L)), na.rm = TRUE)
    list(
      n_total   = n_tot,
      n_journal = n_jour,
      n_doi     = n_doi,
      sci1      = if (n_tot > 0) round(n_jour / n_tot * 100, 1) else NA_real_,
      sci2      = if (n_tot > 0) round(n_doi  / n_tot * 100, 1) else NA_real_
    )
  })

  output$sci_cards <- renderUI({
    s <- sci_scores_rv()
    req(!is.null(s))

    make_gauge <- function(pct, lbl, color) {
      pct_safe <- if (is.na(pct)) 0 else min(100, max(0, pct))
      tags$div(class = "metric-card", style = "min-width:160px;",
        tags$div(class = "metric-val",
                 if (is.na(pct)) "—" else sprintf("%.1f%%", pct)),
        tags$div(class = "metric-lbl", lbl),
        tags$div(class = "sci-bar-wrap",
          tags$div(class = "sci-bar-fill",
                   style = sprintf("width:%.1f%%;background:%s;", pct_safe, color))
        )
      )
    }

    div(class = "metric-row", style = "margin-bottom:8px;",
        make_gauge(s$sci1, paste0("SciScore — journal% (n=", s$n_total, ")"), "#377EB8"),
        make_gauge(s$sci2, paste0("DOI coverage% (n=", s$n_doi, "/", s$n_total, ")"), "#4DAF4A"),
        tags$div(class = "metric-card",
                 tags$div(class = "metric-val", s$n_journal),
                 tags$div(class = "metric-lbl", "Journal refs")),
        tags$div(class = "metric-card",
                 tags$div(class = "metric-val", s$n_doi),
                 tags$div(class = "metric-lbl", "DOI-linked refs"))
    )
  })

  # ── EuropePMC + Google Books annotation ─────────────────────────────────
  epmc_rv <- reactiveVal(NULL)
  isbn_rv <- reactiveVal(NULL)

  # Per-session DOI → EuropePMC record cache. Keyed by lowercased DOI so
  # re-annotation across single-article + corpus tabs reuses lookups.
  epmc_doi_cache <- reactiveVal(list())

  # Per-session ISBN → Google Books record cache. Keyed by the normalised
  # (digits-only / X) form so the single-article and corpus annotators
  # share the cache.
  isbn_cache <- reactiveVal(list())

  # Fast cached + httr-based ISBN annotator.
  #
  # Google Books does NOT support batched OR-queries for ISBN lookups,
  # so the win comes from (1) skipping ISBNs we've already resolved this
  # session and (2) using `httr` with a descriptive User-Agent + retry on
  # transient errors instead of base R's `url()` (which gets throttled).
  # Same return shape as `wikilite::annotate_isbn_google` — one row per
  # successfully-resolved ISBN with columns isbn, title, publisher,
  # publishedDate, categories, authors, description.
  annotate_isbns_google_fast <- function(isbns, use_cache = TRUE,
                                         progress = TRUE,
                                         max_tries = 3L,
                                         base_sleep = 0.6) {
    if (!length(isbns)) return(NULL)
    if (!requireNamespace("httr", quietly = TRUE)) {
      # No httr → fall back to wikilite per-ISBN path.
      rows <- lapply(isbns, function(k) {
        r <- tryCatch(wikilite::annotate_isbn_google(k), error = function(e) NULL)
        if (!is.null(r) && nrow(r) > 0L) { r$isbn <- k; r[1L, , drop = FALSE] }
        else NULL
      })
      return(dplyr::bind_rows(rows))
    }

    norm <- function(x) gsub("[-[:space:]]", "", as.character(x))
    keys <- norm(unique(isbns))
    keys <- keys[nzchar(keys)]
    if (!length(keys)) return(NULL)

    cache <- if (use_cache) isbn_cache() else list()

    out_rows <- list()
    to_fetch <- character(0)
    for (k in keys) {
      if (!is.null(cache[[k]])) out_rows[[k]] <- cache[[k]]
      else to_fetch <- c(to_fetch, k)
    }

    ua <- "wikiliteApp/0.1 (https://github.com/jsobel1/wikiliteApp; jsobel83@gmail.com) R-httr"

    list_field <- function(x) {
      # Google Books returns scalar OR list-of-strings depending on n=1 vs n>1.
      if (is.null(x)) return(NA_character_)
      if (is.list(x)) return(paste(unlist(x[[1]] %||% character(0)), collapse = ", "))
      paste(x, collapse = ", ")
    }
    pick <- function(v) {
      if (is.null(v) || length(v) == 0L) NA_character_
      else as.character(v[[1]])
    }

    if (length(to_fetch) > 0L) {
      n <- length(to_fetch)
      # Google Books has tight burst limits without an API key; pace requests
      # so we don't tip into 429 on the first few.
      throttle <- 0.12
      for (i in seq_along(to_fetch)) {
        k <- to_fetch[i]
        if (progress) {
          tryCatch(
            shiny::incProgress(1 / n, detail = sprintf("%d / %d · %s", i, n, k)),
            error = function(e) NULL
          )
        }
        if (i > 1L) Sys.sleep(throttle)
        url <- paste0("https://www.googleapis.com/books/v1/volumes?q=isbn:", k)
        row <- NULL
        for (attempt in seq_len(max_tries)) {
          resp <- tryCatch(
            httr::GET(url, httr::user_agent(ua), httr::timeout(15)),
            error = function(e) NULL
          )
          if (is.null(resp)) {
            if (attempt < max_tries) Sys.sleep(base_sleep * 2^(attempt - 1L))
            next
          }
          s <- httr::status_code(resp)
          if (s == 200L) {
            body   <- httr::content(resp, "text", encoding = "UTF-8")
            parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE),
                               error = function(e) NULL)
            if (!is.null(parsed) &&
                !is.null(parsed$totalItems) && parsed$totalItems > 0L) {
              vi <- parsed$items$volumeInfo
              row <- data.frame(
                isbn          = k,
                title         = pick(vi$title),
                publisher     = pick(vi$publisher),
                publishedDate = pick(vi$publishedDate),
                categories    = list_field(vi$categories),
                authors       = list_field(vi$authors),
                description   = pick(vi$description),
                stringsAsFactors = FALSE
              )
            }
            break
          } else if (s == 429L || s >= 500L) {
            if (attempt < max_tries) Sys.sleep(base_sleep * 2^(attempt - 1L))
          } else {
            break  # 4xx other than 429: not transient, give up.
          }
        }
        if (!is.null(row)) {
          cache[[k]] <- row
          out_rows[[k]] <- row
        }
      }
      if (use_cache) isbn_cache(cache)
    }

    if (!length(out_rows)) return(NULL)
    out <- do.call(rbind, out_rows)
    rownames(out) <- NULL
    out
  }

  # Fast batched EuropePMC fetcher.
  #
  # Splits the DOI list into chunks of `chunk` DOIs and submits each as a
  # single OR-query to the EuropePMC REST endpoint. Up to two orders of
  # magnitude faster than one-DOI-per-call when annotating a hundred-plus
  # references. Always returns a data.frame with the same columns as
  # `wikilite::annotate_doi_list_europmc`.
  annotate_dois_europmc_fast <- function(dois,
                                         chunk = 25L,
                                         use_cache = TRUE,
                                         progress = TRUE) {
    if (!length(dois)) return(NULL)
    if (!requireNamespace("httr", quietly = TRUE)) {
      # No httr → fall back to the per-DOI wikilite implementation.
      return(tryCatch(wikilite::annotate_doi_list_europmc(dois),
                      error = function(e) NULL))
    }

    required_cols <- c("id", "source", "pmid", "pmcid", "doi",
                       "title", "authorString", "journalTitle",
                       "pubYear", "pubType", "isOpenAccess",
                       "citedByCount", "firstPublicationDate")

    dois_lc <- tolower(unique(trimws(dois)))
    dois_lc <- dois_lc[nzchar(dois_lc)]
    if (!length(dois_lc)) return(NULL)

    cache <- if (use_cache) epmc_doi_cache() else list()

    # Split into already-cached vs. need-to-fetch.
    cached_hits <- list()
    to_fetch    <- character(0)
    for (d in dois_lc) {
      if (!is.null(cache[[d]])) cached_hits[[d]] <- cache[[d]]
      else to_fetch <- c(to_fetch, d)
    }

    ua <- "wikiliteApp/0.1 (https://github.com/jsobel1/wikiliteApp; jsobel83@gmail.com) R-httr"
    base <- "https://www.ebi.ac.uk/europepmc/webservices/rest/search"

    new_rows <- list()
    if (length(to_fetch) > 0L) {
      groups <- split(to_fetch, ceiling(seq_along(to_fetch) / chunk))
      n_g    <- length(groups)
      for (gi in seq_along(groups)) {
        g <- groups[[gi]]
        if (progress) {
          tryCatch(
            shiny::incProgress(1 / n_g,
                               detail = sprintf("batch %d / %d (%d DOIs)",
                                                gi, n_g, length(g))),
            error = function(e) NULL
          )
        }
        # EuropePMC OR-query. URL length stays well under typical limits
        # at chunk = 25 (avg ~25 * 30 = 750 chars).
        q <- paste0("(", paste(sprintf("DOI:%s", g), collapse = " OR "), ")")
        resp <- tryCatch(
          httr::GET(base,
                    query = list(query = q,
                                 format = "json",
                                 pageSize = as.character(min(1000L,
                                                             length(g) * 4L)),
                                 resultType = "lite"),
                    httr::user_agent(ua),
                    httr::timeout(45)),
          error = function(e) NULL
        )
        if (is.null(resp) || httr::status_code(resp) != 200L) next
        body <- httr::content(resp, "text", encoding = "UTF-8")
        parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE),
                           error = function(e) NULL)
        results <- tryCatch(parsed$resultList$result, error = function(e) NULL)
        if (is.null(results) || !is.data.frame(results) ||
            nrow(results) == 0L) next

        # Make sure all required columns exist.
        for (col in required_cols) {
          if (!col %in% names(results)) results[[col]] <- NA
        }
        results <- results[, required_cols, drop = FALSE]
        # Map every record to its lowercased DOI so we can match back.
        results$.key <- tolower(as.character(results$doi))

        # If the API returned multiple records for the same DOI, prefer the
        # exact match over additional hits (EuropePMC sometimes returns
        # the same paper from multiple sources).
        for (d in g) {
          sub <- results[!is.na(results$.key) & results$.key == d, , drop = FALSE]
          if (nrow(sub) == 0L) next
          new_rows[[d]] <- sub[1L, required_cols, drop = FALSE]
        }
      }

      # Persist to session cache.
      if (use_cache && length(new_rows) > 0L) {
        for (k in names(new_rows)) cache[[k]] <- new_rows[[k]]
        epmc_doi_cache(cache)
      }
    }

    out_rows <- c(cached_hits, new_rows)
    if (length(out_rows) == 0L) return(NULL)
    out <- do.call(rbind, out_rows)
    rownames(out) <- NULL
    out
  }

  # Pull a normalised ISBN out of a citation string. Returns "" if none found.
  extract_isbn <- function(x) {
    m <- regmatches(x, regexpr("(?i)isbn\\s*[=:]?\\s*([0-9][0-9X\\- ]{8,})",
                               x, perl = TRUE))
    if (!length(m) || !nzchar(m[[1]])) return(NA_character_)
    raw <- sub("(?i)isbn[^0-9]*", "", m[[1]], perl = TRUE)
    iso <- gsub("[^0-9X]", "", toupper(raw))
    if (nchar(iso) >= 10L) iso else NA_character_
  }

  observeEvent(input$annotate_isbn, { safely({
    cd <- cite_data_rv()
    req(!is.null(cd), !is.null(cd$mat))
    isbns <- unique(na.omit(vapply(cd$mat$cite_raw, extract_isbn, character(1L))))
    if (length(isbns) == 0) {
      showNotification("No ISBNs found for annotation.", type = "warning")
      return()
    }
    withProgress(
      message = sprintf("Querying Google Books for %d ISBNs…", length(isbns)),
      detail  = "0", value = 0,
      {
        res <- tryCatch(annotate_isbns_google_fast(isbns),
                        error = function(e) {
                          showNotification(paste("Google Books error:",
                                                 conditionMessage(e)),
                                           type = "error")
                          NULL
                        })
      }
    )
    isbn_rv(res)
    if (!is.null(res) && nrow(res) > 0)
      showNotification(sprintf("Annotated %d / %d ISBNs.",
                               nrow(res), length(isbns)),
                       type = "message")
    else
      showNotification("No ISBNs matched in Google Books.", type = "warning")
  }, label = "ISBN annotation") })

  observeEvent(input$annotate_epmc, { safely({
    cd <- cite_data_rv()
    req(!is.null(cd), !is.null(cd$mat))
    dois <- unique(na.omit(vapply(cd$mat$cite_raw, function(x) {
      m <- regmatches(x, regexpr("10\\.\\d{4,}/[-._;()/:a-zA-Z0-9]+", x, perl = TRUE))
      if (length(m) && nzchar(m)) m[[1]] else NA_character_
    }, character(1L))))
    if (length(dois) == 0) {
      showNotification("No DOIs found for annotation.", type = "warning")
      return()
    }
    withProgress(
      message = sprintf("Querying EuropePMC for %d DOIs (batched)…",
                        length(dois)),
      detail = "preparing", value = 0,
      {
        res <- tryCatch(annotate_dois_europmc_fast(dois),
                        error = function(e) {
                          showNotification(paste("EuropePMC error:",
                                                 conditionMessage(e)),
                                           type = "error")
                          NULL
                        })
      })
    epmc_rv(res)
    if (!is.null(res))
      showNotification(sprintf("Annotated %d / %d DOIs.",
                               nrow(res), length(dois)),
                       type = "message")
  }, label = "EuropePMC annotation") })

  # ── Citation latency ────────────────────────────────────────────────────
  latency_rv <- reactiveVal(NULL)

  observeEvent(input$compute_latency, { safely({
    cd <- cite_data_rv()
    req(!is.null(cd), !is.null(cd$mat))
    mat <- cd$mat

    doi_rows <- do.call(rbind, lapply(seq_len(nrow(mat)), function(i) {
      m <- regmatches(mat$cite_raw[i],
                      regexpr("10\\.\\d{4,}/[-._;()/:a-zA-Z0-9]+",
                              mat$cite_raw[i], perl = TRUE))
      if (length(m) && nzchar(m[[1]]))
        data.frame(art              = trimws(input$article),
                   citation_fetched = m[[1]],
                   timestamp        = format(mat$first_ts[i], "%Y-%m-%dT%H:%M:%SZ"),
                   stringsAsFactors = FALSE)
      else NULL
    }))

    if (is.null(doi_rows) || nrow(doi_rows) == 0) {
      showNotification("No DOIs found for latency computation.", type = "warning")
      return()
    }

    epmc_ann <- epmc_rv()
    if (is.null(epmc_ann) || nrow(epmc_ann) == 0) {
      withProgress(message = "Querying EuropePMC for latency (batched)…",
                   value = 0, {
        epmc_ann <- tryCatch(
          annotate_dois_europmc_fast(unique(doi_rows$citation_fetched)),
          error = function(e) {
            showNotification(paste("EuropePMC error:", conditionMessage(e)), type = "error")
            NULL
          }
        )
      })
    }

    if (is.null(epmc_ann) || nrow(epmc_ann) == 0) {
      showNotification("EuropePMC returned no results.", type = "warning")
      return()
    }

    lat <- tryCatch(
      wikilite::compute_citation_latency(doi_rows, epmc_ann),
      error = function(e) {
        showNotification(paste("Latency error:", conditionMessage(e)), type = "error")
        NULL
      }
    )
    latency_rv(lat)
    if (!is.null(lat))
      showNotification(
        sprintf("Latency computed for %d citations.", sum(!is.na(lat$latency_days))),
        type = "message"
      )
  }, label = "Citation latency") })

  output$latency_dist_plot <- renderPlot({
    lat <- latency_rv()
    req(!is.null(lat), sum(!is.na(lat$latency_days)) > 1)
    print(wikilite::plot_latency_distribution(lat, stratify_by = "is_preprint"))
  }, res = 96)

  output$latency_seg_plot <- renderPlot({
    lat <- latency_rv()
    req(!is.null(lat), sum(!is.na(lat$latency_days)) > 0)
    print(wikilite::get_segment_history_doi_plot(lat, trimws(input$article)))
  }, res = 96)

  if (has_cr) {
    output$dl_bibtex <- downloadHandler(
      filename = function() {
        art <- gsub("[^A-Za-z0-9_-]", "_", trimws(input$article))
        paste0(art, "_citations.bib")
      },
      content = function(file) {
        cd <- cite_data_rv()
        req(cd)
        dois <- unique(na.omit(vapply(cd$mat$cite_raw, function(x) {
          m <- regmatches(x, regexpr("10\\.\\d{4,}/[-._;()/:a-zA-Z0-9]+", x, perl = TRUE))
          if (length(m) && nzchar(m)) m[[1]] else NA_character_
        }, character(1L))))
        req(length(dois) > 0)
        wikilite::export_doi_to_bib(dois, file)
      }
    )
  }

  # ════════════════════════════════════════════════════════════════════════
  # CORPUS ANALYSIS
  # ════════════════════════════════════════════════════════════════════════

  # corpora_rv: named list. Each element is list(name, label, lang, articles)
  corpora_rv  <- reactiveVal(list())
  corpus_data <- reactiveVal(NULL)   # snapshot of corpora used for the current render

  # ── Add a corpus ────────────────────────────────────────────────────────
  observeEvent(input$corpus_add, {
    name <- trimws(input$new_corpus_name %||% "")
    arts_raw <- trimws(input$new_corpus_articles %||% "")
    if (!nzchar(name)) {
      showNotification("Give the corpus a name.", type = "warning"); return()
    }
    arts <- unique(trimws(strsplit(arts_raw, "\n")[[1]]))
    arts <- arts[nzchar(arts)]
    if (length(arts) == 0) {
      showNotification("Add at least one article.", type = "warning"); return()
    }

    cur <- corpora_rv()
    cur[[name]] <- list(
      name     = name,
      label    = name,
      lang     = input$new_corpus_lang %||% "en",
      articles = arts
    )
    corpora_rv(cur)

    updateTextInput(session, "new_corpus_name", value = "")
    updateTextAreaInput(session, "new_corpus_articles", value = "")
    showNotification(sprintf("Added corpus '%s' (%d articles).", name, length(arts)),
                     type = "message")
  })

  # ── Fetch from a Wikipedia category and pre-fill the new-corpus form ───
  observeEvent(input$corpus_fetch_cat, { safely({
    cat_name <- trimws(input$new_corpus_cat %||% "")
    req(nzchar(cat_name))
    lang <- input$new_corpus_lang %||% "en"
    withProgress(message = paste0("Loading category: ", cat_name, " …"), {
      pages <- tryCatch(
        wikilite::get_pagename_in_cat(cat_name, lang = lang),
        error = function(e) {
          showNotification(paste("Category error:", conditionMessage(e)),
                           type = "error")
          character(0)
        }
      )
    })
    if (length(pages) == 0) {
      showNotification("No articles found in that category.", type = "warning")
      return()
    }
    updateTextAreaInput(session, "new_corpus_articles",
                        value = paste(pages, collapse = "\n"))
    if (!nzchar(input$new_corpus_name %||% "")) {
      updateTextInput(session, "new_corpus_name", value = cat_name)
    }
    showNotification(sprintf("Pre-filled %d articles from category '%s'.",
                             length(pages), cat_name),
                     type = "message")
  }, label = "Category fetch") })

  # ── Remove a corpus ─────────────────────────────────────────────────────
  observeEvent(input$corpus_remove, {
    nm <- input$corpus_remove
    req(nzchar(nm))
    cur <- corpora_rv()
    cur[[nm]] <- NULL
    corpora_rv(cur)
  }, ignoreInit = TRUE)

  # ── Corpus list UI ──────────────────────────────────────────────────────
  output$corpus_list_ui <- renderUI({
    cur <- corpora_rv()
    if (length(cur) == 0) {
      return(p(style = "color:#888; font-size:12px; font-style:italic;",
               "No corpora yet. Add one below."))
    }
    lapply(cur, function(co) {
      nm_js <- jsonlite::toJSON(co$name, auto_unbox = TRUE)
      div(class = "corpus-card",
          tags$div(class = "name", co$name),
          tags$div(class = "meta",
                   sprintf("%d articles · lang: %s", length(co$articles), co$lang)),
          tags$div(class = "arts",
                   paste(head(co$articles, 8), collapse = " · "),
                   if (length(co$articles) > 8)
                     sprintf(" · … (+%d more)", length(co$articles) - 8)),
          tags$div(class = "actions",
            tags$button(
              "Remove", class = "btn btn-default btn-xs",
              onclick = sprintf(
                "Shiny.setInputValue('corpus_remove', %s, {priority:'event'});", nm_js)
            )
          )
      )
    })
  })

  # ── Cross-corpus summary cards ──────────────────────────────────────────
  output$corpus_summary <- renderUI({
    cur <- corpora_rv()
    n_co <- length(cur)
    n_art <- if (n_co > 0) sum(vapply(cur, function(c) length(c$articles), integer(1L))) else 0L
    metric <- function(val, lbl)
      tags$div(class = "metric-card",
               tags$div(class = "metric-val", val),
               tags$div(class = "metric-lbl", lbl))
    div(class = "metric-row",
      metric(n_co,  "Corpora defined"),
      metric(n_art, "Articles total")
    )
  })

  # ── Render comparison: snapshot the current corpora ─────────────────────
  observeEvent(input$corpus_render, {
    cur <- corpora_rv()
    if (length(cur) == 0) {
      showNotification("Define at least one corpus first.", type = "warning"); return()
    }
    all_arts <- unlist(lapply(cur, function(c) c$articles), use.names = FALSE)
    if (length(all_arts) < 2) {
      showNotification("Need at least 2 articles total across corpora.",
                       type = "warning"); return()
    }
    corpus_data(list(
      corpora  = cur,
      lang     = cur[[1]]$lang %||% "en",
      articles = unique(all_arts)
    ))
    showNotification(sprintf("Rendering across %d corpora (%d unique articles).",
                             length(cur), length(unique(all_arts))),
                     type = "message")
  })

  # ── Lightweight article-info fetch (xtools, then MediaWiki fallback) ────
  # Returns one row per article with creation date, creator, edit count,
  # pageviews and assessment — without pulling the full revision history.
  # Cached per-session by (lang, title) so re-renders are instant.
  article_info_cache <- reactiveVal(list())

  fetch_article_info <- function(title, lang = "en") {
    key   <- paste0(lang, "::", title)
    cache <- article_info_cache()
    if (!is.null(cache[[key]])) return(cache[[key]])

    ua <- "wikiliteApp/0.1 (https://github.com/jsobel1/wikiliteApp; jsobel83@gmail.com) R-httr"

    project <- sprintf("%s.wikipedia.org", lang)
    xt_url  <- sprintf("https://xtools.wmcloud.org/api/page/articleinfo/%s/%s",
                       project, utils::URLencode(title, reserved = TRUE))
    info <- tryCatch({
      resp <- httr::GET(xt_url, httr::user_agent(ua), httr::timeout(15))
      if (!is.null(resp) && httr::status_code(resp) == 200L) {
        body <- httr::content(resp, "text", encoding = "UTF-8")
        j <- jsonlite::fromJSON(body, simplifyVector = TRUE)
        list(
          title       = title,
          created_at  = if (!is.null(j$created_at)) as.POSIXct(j$created_at,
                                                               format = "%Y-%m-%dT%H:%M:%SZ",
                                                               tz = "UTC")
                        else as.POSIXct(NA, tz = "UTC"),
          creator     = j$creator %||% NA_character_,
          n_revisions = j$revisions %||% NA_integer_,
          n_editors   = j$editors   %||% NA_integer_,
          pageviews   = j$pageviews %||% NA_integer_,
          watchers    = j$watchers  %||% NA_integer_,
          modified_at = if (!is.null(j$modified_at))
                          as.POSIXct(j$modified_at,
                                     format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
                        else as.POSIXct(NA, tz = "UTC"),
          assessment  = if (is.list(j$assessment)) j$assessment$value %||% NA_character_
                        else NA_character_,
          source      = "xtools"
        )
      } else NULL
    }, error = function(e) NULL)

    # Fallback: minimal MediaWiki call for first revision metadata only.
    if (is.null(info)) {
      mw_url <- sprintf(
        "https://%s.wikipedia.org/w/api.php?action=query&prop=revisions&titles=%s&rvlimit=1&rvdir=newer&rvprop=timestamp%%7Cuser&format=json&formatversion=2",
        lang, utils::URLencode(title, reserved = TRUE)
      )
      info <- tryCatch({
        resp <- httr::GET(mw_url, httr::user_agent(ua), httr::timeout(15))
        if (!is.null(resp) && httr::status_code(resp) == 200L) {
          body <- httr::content(resp, "text", encoding = "UTF-8")
          j <- jsonlite::fromJSON(body, simplifyVector = FALSE)
          rev <- tryCatch(j$query$pages[[1]]$revisions[[1]], error = function(e) NULL)
          if (!is.null(rev)) {
            list(
              title       = title,
              created_at  = as.POSIXct(rev$timestamp,
                                       format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
              creator     = rev$user %||% NA_character_,
              n_revisions = NA_integer_, n_editors = NA_integer_,
              pageviews   = NA_integer_, watchers = NA_integer_,
              modified_at = as.POSIXct(NA, tz = "UTC"),
              assessment  = NA_character_,
              source      = "mediawiki"
            )
          } else NULL
        } else NULL
      }, error = function(e) NULL)
    }

    if (!is.null(info)) {
      cache[[key]] <- info
      article_info_cache(cache)
    }
    info
  }

  # ── Cached latest-revision wikitext fetch ────────────────────────────────
  # Used by the co-citation / publication / wikilink network panels so they
  # don't re-fetch every render. Returns a character scalar (full wikitext
  # of the most recent revision) or NULL.
  article_wikitext_cache <- reactiveVal(list())

  fetch_article_latest_wikitext <- function(title, lang = "en") {
    key   <- paste0(lang, "::", title)
    cache <- article_wikitext_cache()
    if (!is.null(cache[[key]])) return(cache[[key]])

    if (!requireNamespace("httr", quietly = TRUE)) return(NULL)
    ua <- "wikiliteApp/0.1 (https://github.com/jsobel1/wikiliteApp; jsobel83@gmail.com) R-httr"
    url <- sprintf(
      "https://%s.wikipedia.org/w/api.php?action=query&titles=%s&prop=revisions&rvprop=content&rvslots=main&rvlimit=1&format=json&formatversion=2",
      lang, utils::URLencode(title, reserved = TRUE)
    )
    wt <- tryCatch({
      resp <- httr::GET(url, httr::user_agent(ua), httr::timeout(20))
      if (is.null(resp) || httr::status_code(resp) != 200L) NULL
      else {
        body <- httr::content(resp, "text", encoding = "UTF-8")
        j <- jsonlite::fromJSON(body, simplifyVector = FALSE)
        tryCatch(j$query$pages[[1]]$revisions[[1]]$slots$main$content,
                 error = function(e) NULL)
      }
    }, error = function(e) NULL)

    if (!is.null(wt)) {
      cache[[key]] <- wt
      article_wikitext_cache(cache)
    }
    wt
  }

  # ── DOI/wikilink extraction for the corpus ──────────────────────────────
  # Cached: articles already in `article_wikitext_cache` are skipped. Returns
  # a long data frame: (corpus, article, doi) — used by the co-citation tab.
  # ── Corpus citations panel ──────────────────────────────────────────────
  # Triggered by the "Extract citations" button so the user controls when
  # the per-article wikitext fetch runs (cached, but still N HTTP calls
  # the first time). Returns one row per (corpus, article, citation).
  corpus_citations_rv <- reactiveVal(NULL)
  corpus_epmc_rv      <- reactiveVal(NULL)
  corpus_isbn_rv      <- reactiveVal(NULL)

  observeEvent(input$corpus_extract_cites, { safely({
    d <- corpus_data()
    req(!is.null(d))

    flat <- do.call(rbind, lapply(d$corpora, function(c) {
      data.frame(corpus = c$name, article = c$articles,
                 stringsAsFactors = FALSE)
    }))
    flat <- unique(flat)
    if (nrow(flat) == 0L) {
      showNotification("Corpus is empty.", type = "warning"); return()
    }

    rows <- list()
    n <- nrow(flat)
    withProgress(message = "Extracting citations from corpus…",
                 detail = sprintf("0 / %d", n), value = 0, {
      for (i in seq_len(n)) {
        a <- flat$article[i]
        incProgress(1 / n, detail = sprintf("%d / %d · %s", i, n, a))
        wt <- fetch_article_latest_wikitext(a, d$lang)
        if (is.null(wt) || !nzchar(wt)) next
        cites <- extract_cites(wt)
        if (length(cites) == 0L) next
        rows[[length(rows) + 1L]] <- data.frame(
          corpus    = flat$corpus[i],
          article   = a,
          cite_raw  = cites,
          stringsAsFactors = FALSE
        )
      }
    })

    if (length(rows) == 0L) {
      showNotification("No <ref> citations found in this corpus.",
                       type = "warning"); return()
    }
    df <- do.call(rbind, rows)
    df$type  <- vapply(df$cite_raw, cite_type,  character(1L))
    df$label <- vapply(df$cite_raw, cite_label, character(1L))
    df$doi   <- vapply(df$cite_raw, function(x) {
      m <- regmatches(x, regexpr("10\\.\\d{4,}/[-._;()/:a-zA-Z0-9]+",
                                 x, perl = TRUE))
      if (length(m) && nzchar(m)) m[[1]] else NA_character_
    }, character(1L))
    df$isbn  <- vapply(df$cite_raw, extract_isbn, character(1L))
    df$url   <- vapply(df$cite_raw,
                       function(x) cite_url(x) %||% "",
                       character(1L))

    corpus_citations_rv(df)
    corpus_epmc_rv(NULL)   # invalidate stale annotations
    corpus_isbn_rv(NULL)
    showNotification(sprintf(
      "Extracted %d citations across %d articles.",
      nrow(df), length(unique(df$article))),
      type = "message")
  }, label = "Corpus citation extraction") })

  observeEvent(input$corpus_annotate_epmc, { safely({
    df <- corpus_citations_rv()
    req(!is.null(df), nrow(df) > 0)
    dois <- unique(na.omit(df$doi))
    if (length(dois) == 0L) {
      showNotification("No DOIs in corpus citations.", type = "warning")
      return()
    }
    withProgress(message = sprintf("Querying EuropePMC for %d DOIs (batched)…",
                                   length(dois)), value = 0, {
      res <- tryCatch(annotate_dois_europmc_fast(dois),
                      error = function(e) {
                        showNotification(paste("EuropePMC error:",
                                               conditionMessage(e)),
                                         type = "error"); NULL })
    })
    corpus_epmc_rv(res)
    if (!is.null(res))
      showNotification(sprintf("Annotated %d / %d DOIs.",
                               nrow(res), length(dois)),
                       type = "message")
  }, label = "Corpus EuropePMC annotation") })

  observeEvent(input$corpus_annotate_isbn, { safely({
    df <- corpus_citations_rv()
    req(!is.null(df), nrow(df) > 0)
    isbns <- unique(na.omit(df$isbn))
    if (length(isbns) == 0L) {
      showNotification("No ISBNs in corpus citations.", type = "warning")
      return()
    }
    withProgress(message = sprintf("Querying Google Books for %d ISBNs…",
                                   length(isbns)),
                 detail = "0", value = 0, {
      res <- tryCatch(annotate_isbns_google_fast(isbns),
                      error = function(e) {
                        showNotification(paste("Google Books error:",
                                               conditionMessage(e)),
                                         type = "error")
                        NULL
                      })
    })
    corpus_isbn_rv(res)
    if (!is.null(res) && nrow(res) > 0)
      showNotification(sprintf("Annotated %d / %d ISBNs.",
                               nrow(res), length(isbns)),
                       type = "message")
    else
      showNotification("No ISBNs matched in Google Books.",
                       type = "warning")
  }, label = "Corpus ISBN annotation") })

  # SciScore per article (within each corpus). One row per article.
  corpus_article_sci_summary <- reactive({
    df <- corpus_citations_rv()
    req(!is.null(df), nrow(df) > 0)
    df %>%
      dplyr::group_by(corpus, article) %>%
      dplyr::summarise(
        n_total   = dplyr::n(),
        n_journal = sum(type == "Journal", na.rm = TRUE),
        n_doi     = sum(!is.na(doi), na.rm = TRUE),
        n_isbn    = sum(!is.na(isbn), na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      dplyr::mutate(
        journal_pct = round(100 * n_journal / pmax(n_total, 1L), 1),
        doi_pct     = round(100 * n_doi     / pmax(n_total, 1L), 1)
      ) %>%
      dplyr::arrange(corpus, dplyr::desc(journal_pct)) %>%
      as.data.frame()
  })

  # SciScore per corpus
  corpus_sci_summary <- reactive({
    df <- corpus_citations_rv()
    req(!is.null(df), nrow(df) > 0)
    df %>%
      dplyr::group_by(corpus) %>%
      dplyr::summarise(
        n_total   = dplyr::n(),
        n_journal = sum(type == "Journal", na.rm = TRUE),
        n_doi     = sum(!is.na(doi), na.rm = TRUE),
        n_isbn    = sum(!is.na(isbn), na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      dplyr::mutate(
        journal_pct = round(100 * n_journal / pmax(n_total, 1L), 1),
        doi_pct     = round(100 * n_doi     / pmax(n_total, 1L), 1)
      ) %>%
      as.data.frame()
  })

  output$corpus_sci_cards <- renderUI({
    s <- tryCatch(corpus_sci_summary(), error = function(e) NULL)
    if (is.null(s) || nrow(s) == 0L) {
      return(div(class = "metric-row",
        tags$div(class = "metric-card", style = "min-width:260px;",
          tags$div(class = "metric-val", "—"),
          tags$div(class = "metric-lbl",
                   "Click 'Extract citations from corpus' to populate"))
      ))
    }
    metric <- function(val, lbl)
      tags$div(class = "metric-card",
               tags$div(class = "metric-val", val),
               tags$div(class = "metric-lbl", lbl))

    totals <- list(
      n_tot   = sum(s$n_total),
      n_jr    = sum(s$n_journal),
      n_doi   = sum(s$n_doi),
      n_isbn  = sum(s$n_isbn)
    )
    div(class = "metric-row",
      metric(format(totals$n_tot,  big.mark = ","), "Citations total"),
      metric(format(totals$n_jr,   big.mark = ","), "Journal refs"),
      metric(format(totals$n_doi,  big.mark = ","), "DOI-linked"),
      metric(format(totals$n_isbn, big.mark = ","), "ISBN-linked"),
      metric(sprintf("%.1f%%", 100 * totals$n_jr / max(totals$n_tot, 1L)),
             "Journal % overall"),
      metric(sprintf("%.1f%%", 100 * totals$n_doi / max(totals$n_tot, 1L)),
             "DOI % overall")
    )
  })

  output$corpus_cite_type_plot <- renderPlotly({
    df <- corpus_citations_rv()
    req(!is.null(df), nrow(df) > 0)

    view <- input$corpus_type_view %||% "corpus"

    # ── Per-article stacked composition ──────────────────────────────────
    if (identical(view, "article")) {
      max_n <- max(5L, as.integer(input$corpus_type_max_articles %||% 60L))
      norm  <- input$corpus_type_normalise %||% "counts"

      art_totals <- df %>%
        dplyr::group_by(corpus, article) %>%
        dplyr::summarise(n_total = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(n_total))

      total_arts <- nrow(art_totals)
      keep <- art_totals[seq_len(min(max_n, total_arts)), , drop = FALSE]

      counts <- df %>%
        dplyr::semi_join(keep, by = c("corpus", "article")) %>%
        dplyr::count(corpus, article, type) %>%
        dplyr::left_join(keep, by = c("corpus", "article")) %>%
        dplyr::mutate(
          pct  = round(100 * n / pmax(n_total, 1L), 1),
          type = factor(type, levels = names(type_pal))
        )

      # Y-axis: keep the same "by total citations" order used to filter.
      y_levels <- rev(keep$article)
      counts$article <- factor(counts$article, levels = y_levels)

      x_var <- if (norm == "pct") counts$pct else counts$n
      x_lab <- if (norm == "pct") "% of citations" else "Citations"

      hover <- sprintf(
        "<b>%s</b><br>Corpus: %s<br>%s: %s%s<br>Total citations: %d",
        htmltools::htmlEscape(as.character(counts$article)),
        htmltools::htmlEscape(counts$corpus),
        counts$type,
        if (norm == "pct") sprintf("%.1f", counts$pct) else format(counts$n),
        if (norm == "pct") "%" else "",
        counts$n_total
      )

      fig <- plotly::plot_ly(
        x = x_var, y = counts$article, color = counts$type,
        colors = type_pal, type = "bar", orientation = "h",
        text = hover, hoverinfo = "text"
      )

      h_px <- max(360L, min(1500L, length(y_levels) * 18L + 120L))

      return(
        fig %>% plotly::layout(
          title = list(text = sprintf(
            "<b>Reference composition per article — %d of %d articles</b>",
            length(y_levels), total_arts),
            font = list(size = 12)),
          height = h_px,
          barmode = "stack",
          xaxis = list(title = x_lab,
                       ticksuffix = if (norm == "pct") "%" else ""),
          yaxis = list(title = "", automargin = TRUE,
                       tickfont = list(size = 10)),
          margin = list(l = 200, r = 30, t = 50, b = 40),
          legend = list(orientation = "h", x = 0, y = -0.10,
                        title = list(text = "Reference type"))
        )
      )
    }

    # ── Default: per-corpus stacked ──────────────────────────────────────
    counts <- df %>%
      dplyr::count(corpus, type) %>%
      dplyr::mutate(type = factor(type, levels = names(type_pal)))

    plotly::plot_ly(
      counts, x = ~corpus, y = ~n, color = ~type,
      colors = type_pal, type = "bar",
      hovertemplate = paste0("Corpus: %{x}<br>",
                             "%{fullData.name}: %{y}<extra></extra>")
    ) %>% plotly::layout(
      title = list(
        text = sprintf("<b>%s citations across %d corpora</b>",
                       format(nrow(df), big.mark = ","),
                       length(unique(counts$corpus))),
        font = list(size = 12)),
      barmode = "stack",
      xaxis = list(title = ""),
      yaxis = list(title = "Citations"),
      legend = list(orientation = "h", x = 0, y = -0.18,
                    title = list(text = "Reference type"))
    )
  })

  output$corpus_sci_bar_plot <- renderPlotly({
    s <- tryCatch(corpus_sci_summary(), error = function(e) NULL)
    req(!is.null(s), nrow(s) > 0)

    s_long <- s %>%
      dplyr::select(corpus, journal_pct, doi_pct) %>%
      tidyr::pivot_longer(-corpus, names_to = "metric", values_to = "pct") %>%
      dplyr::mutate(metric = ifelse(metric == "journal_pct",
                                    "Journal %", "DOI %"))

    plotly::plot_ly(
      s_long, x = ~pct, y = ~corpus, color = ~metric,
      colors = c("Journal %" = "#377EB8", "DOI %" = "#4DAF4A"),
      type = "bar", orientation = "h",
      text = ~sprintf("%.1f%%", pct), textposition = "outside",
      hovertemplate = paste0("%{fullData.name}: %{x:.1f}%<extra></extra>")
    ) %>% plotly::layout(
      barmode = "group",
      xaxis = list(title = "% of citations", range = c(0, 105),
                   ticksuffix = "%"),
      yaxis = list(title = ""),
      legend = list(orientation = "h", x = 0, y = -0.15)
    )
  })

  # Per-article SciScore — switchable view.
  output$corpus_sci_article_plot <- renderPlotly({
    art <- tryCatch(corpus_article_sci_summary(),
                    error = function(e) NULL)
    req(!is.null(art), nrow(art) > 0)

    base_pal <- c("#377EB8","#E41A1C","#4DAF4A","#984EA3","#FF7F00",
                  "#A65628","#F781BF","#66C2A5","#FFD92F","#8DA0CB")
    cn_levels <- unique(art$corpus)
    pal <- setNames(rep(base_pal, length.out = length(cn_levels)), cn_levels)

    art$hover <- sprintf(
      "<b>%s</b><br>Corpus: %s<br>Journal %%: %.1f%% · DOI %%: %.1f%%<br>%d citations",
      htmltools::htmlEscape(art$article),
      htmltools::htmlEscape(art$corpus),
      art$journal_pct, art$doi_pct, art$n_total
    )

    view <- input$corpus_sci_view %||% "box"

    # ── Per-article horizontal bar chart ─────────────────────────────────
    if (identical(view, "bars")) {
      metric <- input$corpus_sci_metric %||% "journal_pct"
      max_n  <- max(5L, as.integer(input$corpus_sci_max_articles %||% 60L))

      art$score <- art[[metric]]
      art <- art[order(-art$score), , drop = FALSE]
      total_articles <- nrow(art)
      truncated <- total_articles > max_n
      if (truncated) art <- art[seq_len(max_n), , drop = FALSE]

      # Y-axis order: highest score on top.
      art$y_label <- factor(art$article,
                            levels = rev(art$article))

      metric_label <- if (metric == "doi_pct") "DOI %" else "Journal %"

      fig <- plotly::plot_ly()
      for (cn in cn_levels) {
        sub <- art[art$corpus == cn, , drop = FALSE]
        if (nrow(sub) == 0L) next
        fig <- fig %>%
          plotly::add_trace(
            y = sub$y_label,
            x = sub$score,
            type = "bar", orientation = "h",
            marker = list(color = pal[[cn]],
                          line = list(width = 0.5, color = "#fff")),
            name = cn, legendgroup = cn,
            text = sprintf("%.1f%% · n=%d", sub$score, sub$n_total),
            textposition = "outside",
            hovertext = sub$hover, hoverinfo = "text"
          )
      }

      title_txt <- sprintf(
        "<b>Per-article %s — sorted (showing %d of %d)</b>",
        metric_label, nrow(art), total_articles
      )

      # Bar chart height scales with #articles so labels stay readable.
      h_px <- max(360L, min(1500L, nrow(art) * 18L + 120L))

      return(
        fig %>% plotly::layout(
          title = list(text = title_txt, font = list(size = 12)),
          height = h_px,
          xaxis = list(title = sprintf("%s of citations", metric_label),
                       range = c(0, 105), ticksuffix = "%"),
          yaxis = list(title = "", automargin = TRUE,
                       tickfont = list(size = 10)),
          legend = list(orientation = "h", x = 0, y = -0.08,
                        title = list(text = "Corpus")),
          margin = list(l = 200, r = 60, t = 50, b = 40)
        )
      )
    }

    # ── Default: box plot per corpus with article points ─────────────────
    corp_summary <- tryCatch(corpus_sci_summary(), error = function(e) NULL)

    fig <- plotly::plot_ly()
    for (cn in cn_levels) {
      sub <- art[art$corpus == cn, , drop = FALSE]
      fig <- fig %>%
        plotly::add_trace(
          y = sub$journal_pct,
          x = rep(cn, nrow(sub)),
          type = "box", boxpoints = "all",
          jitter = 0.5, pointpos = 0,
          marker = list(color = pal[[cn]], size = 7,
                        line = list(width = 1, color = "#fff")),
          line   = list(color = pal[[cn]]),
          fillcolor = paste0(pal[[cn]], "33"),
          name = cn, legendgroup = cn,
          text = sub$hover, hoverinfo = "text"
        )
      if (!is.null(corp_summary) && cn %in% corp_summary$corpus) {
        m <- corp_summary$journal_pct[corp_summary$corpus == cn]
        fig <- fig %>%
          plotly::add_trace(
            y = m, x = cn, type = "scatter", mode = "markers",
            marker = list(symbol = "diamond", size = 14,
                          color = "#000", opacity = 0.7,
                          line = list(width = 1, color = "#fff")),
            name = paste0(cn, " (corpus mean)"),
            legendgroup = cn, showlegend = FALSE,
            hovertemplate = sprintf(
              "<b>%s</b><br>Corpus mean Journal %%: %.1f%%<extra></extra>",
              cn, m)
          )
      }
    }

    fig %>% plotly::layout(
      title = list(text = sprintf(
        "<b>Per-article journal %% — %d articles, %d corpora</b>",
        nrow(art), length(cn_levels)),
        font = list(size = 12)),
      yaxis = list(title = "Journal % of citations",
                   range = c(0, 105), ticksuffix = "%"),
      xaxis = list(title = ""),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.18)
    )
  })

  # Per-article SciScore DT
  if (has_DT) {
    output$corpus_sci_article_table <- DT::renderDT({
      a <- tryCatch(corpus_article_sci_summary(), error = function(e) NULL)
      req(!is.null(a), nrow(a) > 0)
      a %>%
        dplyr::transmute(
          Corpus = corpus, Article = article,
          Citations = n_total,
          `Journal #` = n_journal, `Journal %` = journal_pct,
          `DOI #` = n_doi, `DOI %` = doi_pct,
          `ISBN #` = n_isbn
        )
    },
    options = list(pageLength = 12, scrollX = TRUE, dom = "ftp"),
    filter = "top", rownames = FALSE,
    class = "compact stripe hover")
  } else {
    output$corpus_sci_article_table_base <- renderTable({
      a <- tryCatch(corpus_article_sci_summary(), error = function(e) NULL)
      req(!is.null(a), nrow(a) > 0); a
    })
  }

  # Build the rich citation table (with annotations folded in).
  corpus_cite_table_df <- reactive({
    df <- corpus_citations_rv()
    req(!is.null(df), nrow(df) > 0)
    epmc <- corpus_epmc_rv()
    isbn <- corpus_isbn_rv()
    out <- df %>% dplyr::mutate(
      Citation = mapply(make_cite_link, label, url),
      Type     = type
    )
    if (!is.null(epmc) && nrow(epmc) > 0L) {
      idx <- match(out$doi, epmc$doi)
      out$Journal   <- epmc$journalTitle[idx]
      out$Year      <- epmc$pubYear[idx]
      out$OA        <- ifelse(!is.na(idx) & epmc$isOpenAccess[idx] == "Y", "OA ✓", "")
      out$Citations <- epmc$citedByCount[idx]
    }
    if (!is.null(isbn) && nrow(isbn) > 0L) {
      idx2 <- match(out$isbn, isbn$isbn)
      out$`Book title` <- ifelse(is.na(idx2), NA_character_, isbn$title[idx2])
      out$Publisher    <- ifelse(is.na(idx2), NA_character_, isbn$publisher[idx2])
      out$Authors      <- ifelse(is.na(idx2), NA_character_, isbn$authors[idx2])
    }
    cols <- c("corpus", "article", "Citation", "Type")
    if (!is.null(epmc) && nrow(epmc) > 0L)
      cols <- c(cols, "Journal", "Year", "OA", "Citations")
    if (!is.null(isbn) && nrow(isbn) > 0L)
      cols <- c(cols, "Book title", "Publisher", "Authors")
    cols <- c(cols, "doi", "isbn")
    out <- out[, intersect(cols, names(out)), drop = FALSE]
    names(out)[names(out) == "corpus"]  <- "Corpus"
    names(out)[names(out) == "article"] <- "Article"
    names(out)[names(out) == "doi"]     <- "DOI"
    names(out)[names(out) == "isbn"]    <- "ISBN"
    out
  })

  if (has_DT) {
    output$corpus_cite_table <- DT::renderDT({
      corpus_cite_table_df()
    },
    escape   = FALSE,
    options  = list(pageLength = 12, scrollX = TRUE, dom = "ftp"),
    filter   = "top", rownames = FALSE,
    class    = "compact stripe hover")
  } else {
    output$corpus_cite_table_base <- renderTable({ corpus_cite_table_df() })
  }

  if (has_xlsx) {
    output$dl_corpus_citations_xlsx <- downloadHandler(
      filename = function() "corpus_citations.xlsx",
      content = function(file) {
        df   <- corpus_citations_rv()
        epmc <- corpus_epmc_rv()
        isbn <- corpus_isbn_rv()
        req(!is.null(df), nrow(df) > 0)
        out <- df %>%
          dplyr::transmute(
            Corpus  = corpus, Article = article,
            Citation = label, Type = type,
            URL = url, DOI = doi, ISBN = isbn,
            `Raw wikitext` = cite_raw
          )
        if (!is.null(epmc) && nrow(epmc) > 0L) {
          idx <- match(out$DOI, epmc$doi)
          out$Journal   <- epmc$journalTitle[idx]
          out$Year      <- epmc$pubYear[idx]
          out$OA        <- ifelse(!is.na(idx) & epmc$isOpenAccess[idx] == "Y",
                                  "OA", "")
          out$Citations <- epmc$citedByCount[idx]
        }
        if (!is.null(isbn) && nrow(isbn) > 0L) {
          idx2 <- match(out$ISBN, isbn$isbn)
          out$`Book title` <- ifelse(is.na(idx2), NA_character_, isbn$title[idx2])
          out$Publisher    <- ifelse(is.na(idx2), NA_character_, isbn$publisher[idx2])
          out$Authors      <- ifelse(is.na(idx2), NA_character_, isbn$authors[idx2])
        }
        openxlsx::write.xlsx(out, file)
      }
    )

    output$dl_corpus_summary_xlsx <- downloadHandler(
      filename = function() "corpus_sciscore_per_corpus.xlsx",
      content = function(file) {
        s <- tryCatch(corpus_sci_summary(), error = function(e) NULL)
        df <- corpus_citations_rv()
        req(!is.null(s), nrow(s) > 0, !is.null(df))
        type_break <- df %>%
          dplyr::count(corpus, type) %>%
          tidyr::pivot_wider(names_from = type, values_from = n,
                             values_fill = 0L) %>%
          as.data.frame()
        sheets <- list(
          `Corpus summary` = s,
          `Counts by type` = type_break
        )
        openxlsx::write.xlsx(sheets, file)
      }
    )

    # ── Standalone annotation tables ───────────────────────────────────
    # Each writes the *raw* third-party annotation table joined to the
    # corpus/article context where each identifier was found, so the
    # spreadsheet is self-contained and traceable.

    output$dl_corpus_epmc_xlsx <- downloadHandler(
      filename = function() "corpus_doi_annotations_europepmc.xlsx",
      content = function(file) {
        epmc <- corpus_epmc_rv()
        df   <- corpus_citations_rv()
        if (is.null(epmc) || nrow(epmc) == 0L) {
          showNotification(
            "No DOI annotations yet — click 'Annotate DOIs (EuropePMC)' first.",
            type = "warning")
          openxlsx::write.xlsx(
            data.frame(Message = "No EuropePMC annotations to export."),
            file)
          return()
        }
        # Where each annotated DOI appears in the corpus.
        usage <- if (!is.null(df) && nrow(df) > 0L) {
          df %>%
            dplyr::filter(!is.na(doi)) %>%
            dplyr::group_by(doi) %>%
            dplyr::summarise(
              n_articles = dplyr::n_distinct(article),
              corpora    = paste(sort(unique(corpus)), collapse = "; "),
              articles   = paste(sort(unique(article)), collapse = "; "),
              .groups    = "drop"
            ) %>% as.data.frame()
        } else NULL
        sheets <- list(`EuropePMC annotations` = epmc)
        if (!is.null(usage)) sheets[["Where used in corpus"]] <- usage
        openxlsx::write.xlsx(sheets, file)
      }
    )

    output$dl_corpus_isbn_xlsx <- downloadHandler(
      filename = function() "corpus_isbn_annotations_google_books.xlsx",
      content = function(file) {
        isbn <- corpus_isbn_rv()
        df   <- corpus_citations_rv()
        if (is.null(isbn) || nrow(isbn) == 0L) {
          showNotification(
            "No ISBN annotations yet — click 'Annotate ISBNs (Google Books)' first.",
            type = "warning")
          openxlsx::write.xlsx(
            data.frame(Message = "No Google Books annotations to export."),
            file)
          return()
        }
        usage <- if (!is.null(df) && nrow(df) > 0L) {
          df %>%
            dplyr::filter(!is.na(isbn)) %>%
            dplyr::group_by(isbn) %>%
            dplyr::summarise(
              n_articles = dplyr::n_distinct(article),
              corpora    = paste(sort(unique(corpus)), collapse = "; "),
              articles   = paste(sort(unique(article)), collapse = "; "),
              .groups    = "drop"
            ) %>% as.data.frame()
        } else NULL
        sheets <- list(`Google Books annotations` = isbn)
        if (!is.null(usage)) sheets[["Where used in corpus"]] <- usage
        openxlsx::write.xlsx(sheets, file)
      }
    )

    output$dl_corpus_sci_article_xlsx <- downloadHandler(
      filename = function() "corpus_sciscore_per_article.xlsx",
      content = function(file) {
        a <- tryCatch(corpus_article_sci_summary(),
                      error = function(e) NULL)
        req(!is.null(a), nrow(a) > 0)
        out <- a %>% dplyr::transmute(
          Corpus      = corpus,
          Article     = article,
          Citations   = n_total,
          `Journal #` = n_journal, `Journal %` = journal_pct,
          `DOI #`     = n_doi,     `DOI %`     = doi_pct,
          `ISBN #`    = n_isbn
        )
        openxlsx::write.xlsx(out, file)
      }
    )

    # All SciScore views as a multi-sheet workbook for easy sharing.
    output$dl_corpus_sci_combined_xlsx <- downloadHandler(
      filename = function() "corpus_sciscore_all.xlsx",
      content = function(file) {
        a <- tryCatch(corpus_article_sci_summary(),
                      error = function(e) NULL)
        s <- tryCatch(corpus_sci_summary(), error = function(e) NULL)
        df <- corpus_citations_rv()
        req(!is.null(a), nrow(a) > 0,
            !is.null(s), nrow(s) > 0,
            !is.null(df))

        # Compute cross-corpus per-article-distribution stats (median, IQR,
        # min/max of journal % across articles within each corpus).
        cross <- a %>%
          dplyr::group_by(corpus) %>%
          dplyr::summarise(
            n_articles      = dplyr::n(),
            mean_journal    = round(mean(journal_pct, na.rm = TRUE), 1),
            median_journal  = round(stats::median(journal_pct, na.rm = TRUE), 1),
            iqr_journal     = round(stats::IQR(journal_pct, na.rm = TRUE), 1),
            min_journal     = round(min(journal_pct, na.rm = TRUE), 1),
            max_journal     = round(max(journal_pct, na.rm = TRUE), 1),
            mean_doi        = round(mean(doi_pct, na.rm = TRUE), 1),
            median_doi      = round(stats::median(doi_pct, na.rm = TRUE), 1),
            .groups = "drop"
          ) %>% as.data.frame()

        type_break <- df %>%
          dplyr::count(corpus, type) %>%
          tidyr::pivot_wider(names_from = type, values_from = n,
                             values_fill = 0L) %>%
          as.data.frame()

        sheets <- list(
          `SciScore per corpus`   = s,
          `SciScore per article`  = a %>% dplyr::transmute(
            Corpus = corpus, Article = article,
            Citations = n_total,
            `Journal #` = n_journal, `Journal %` = journal_pct,
            `DOI #`     = n_doi,     `DOI %`     = doi_pct,
            `ISBN #`    = n_isbn
          ),
          `Cross-corpus stats`    = cross,
          `Counts by type`        = type_break
        )
        openxlsx::write.xlsx(sheets, file)
      }
    )
  }

  corpus_doi_df_rv <- reactive({
    d <- corpus_data()
    req(!is.null(d))

    flat <- do.call(rbind, lapply(d$corpora, function(c) {
      data.frame(corpus = c$name, article = c$articles,
                 stringsAsFactors = FALSE)
    }))
    flat <- unique(flat)
    if (nrow(flat) == 0L) return(NULL)

    doi_re <- "10\\.\\d{4,}/[-._;()/:a-zA-Z0-9]+"
    rows <- list()
    n <- nrow(flat)

    withProgress(message = "Fetching article wikitexts (cached)…",
                 detail = sprintf("0 / %d", n), value = 0, {
      for (i in seq_len(n)) {
        a <- flat$article[i]
        incProgress(1 / n, detail = sprintf("%d / %d · %s", i, n, a))
        wt <- fetch_article_latest_wikitext(a, d$lang)
        if (is.null(wt) || !nzchar(wt)) next
        m <- regmatches(wt, gregexpr(doi_re, wt, perl = TRUE))[[1]]
        m <- unique(tolower(m))
        if (length(m) == 0L) next
        rows[[length(rows) + 1L]] <- data.frame(
          corpus  = flat$corpus[i],
          article = a,
          doi     = m,
          stringsAsFactors = FALSE
        )
      }
    })

    if (length(rows) == 0L) return(NULL)
    do.call(rbind, rows)
  })

  # ── Wikilink extraction (cached) ────────────────────────────────────────
  # Returns a long data frame: (corpus, article, target). Skips namespaced
  # links (File:, Image:, Category:, Wikipedia:, etc.) and self-links.
  # Re-uses the same article_wikitext_cache as the DOI extractor, so
  # opening the Wikilink tab after Co-citation / Publication is instant.
  corpus_wikilink_df_rv <- reactive({
    d <- corpus_data()
    req(!is.null(d))

    flat <- do.call(rbind, lapply(d$corpora, function(c) {
      data.frame(corpus = c$name, article = c$articles,
                 stringsAsFactors = FALSE)
    }))
    flat <- unique(flat)
    if (nrow(flat) == 0L) return(NULL)

    skip_ns <- "^(File|Image|Category|Wikipedia|Help|Portal|Template|Module|MediaWiki|Special|User|Talk):"
    rows <- list()
    n <- nrow(flat)

    withProgress(message = "Extracting wikilinks (cached wikitext)…",
                 detail = sprintf("0 / %d", n), value = 0, {
      for (i in seq_len(n)) {
        a <- flat$article[i]
        incProgress(1 / n, detail = sprintf("%d / %d · %s", i, n, a))
        wt <- fetch_article_latest_wikitext(a, d$lang)
        if (is.null(wt) || !nzchar(wt)) next

        # Match [[target]] / [[target|display]] / [[target#section]]
        m <- regmatches(wt, gregexpr("\\[\\[[^\\[\\]\\n]+\\]\\]",
                                     wt, perl = TRUE))[[1]]
        if (!length(m)) next
        inner <- sub("^\\[\\[", "", sub("\\]\\]$", "", m))
        # Strip display label (after |) and section anchor (after #).
        targets <- sub("\\|.*$", "", inner)
        targets <- sub("#.*$", "", targets)
        # Normalise: trim, replace underscores with spaces, capitalize first letter.
        targets <- trimws(gsub("_", " ", targets))
        targets <- ifelse(nzchar(targets),
                          paste0(toupper(substr(targets, 1, 1)),
                                 substr(targets, 2, nchar(targets))),
                          targets)
        # Drop empties, namespaced links, self-links.
        targets <- targets[nzchar(targets)]
        targets <- targets[!grepl(skip_ns, targets, ignore.case = TRUE)]
        targets <- targets[targets != a]
        targets <- unique(targets)
        if (!length(targets)) next

        rows[[length(rows) + 1L]] <- data.frame(
          corpus  = flat$corpus[i],
          article = a,
          target  = targets,
          stringsAsFactors = FALSE
        )
      }
    })

    if (length(rows) == 0L) return(NULL)
    do.call(rbind, rows)
  })

  # Build a tidy data frame: one row per (corpus, article) with fetched info.
  corpus_article_info_df <- reactive({
    d <- corpus_data()
    req(!is.null(d))
    rows <- list()
    n <- sum(vapply(d$corpora, function(c) length(c$articles), integer(1L)))
    if (n == 0L) return(NULL)

    withProgress(message = "Fetching article info…",
                 detail = sprintf("0 / %d", n), value = 0, {
      i <- 0L
      for (corpus in d$corpora) {
        for (a in corpus$articles) {
          i <- i + 1L
          incProgress(1 / n, detail = sprintf("%d / %d · %s", i, n, a))
          info <- fetch_article_info(a, d$lang)
          if (is.null(info)) next
          rows[[length(rows) + 1L]] <- data.frame(
            corpus       = corpus$name,
            article      = a,
            created_at   = info$created_at,
            creator      = info$creator,
            n_revisions  = info$n_revisions,
            n_editors    = info$n_editors,
            pageviews    = info$pageviews,
            watchers     = info$watchers,
            modified_at  = info$modified_at,
            assessment   = info$assessment %||% NA_character_,
            stringsAsFactors = FALSE
          )
        }
      }
    })
    if (length(rows) == 0L) return(NULL)
    do.call(rbind, rows)
  })

  output$corpus_timeline <- renderPlotly({
    d  <- corpus_data()
    df <- corpus_article_info_df()
    req(!is.null(d), !is.null(df), nrow(df) > 0)

    df <- df[!is.na(df$created_at), , drop = FALSE]
    if (nrow(df) == 0L) {
      showNotification("No creation dates resolved for this corpus.",
                       type = "warning")
      return(plotly::plotly_empty())
    }

    y_mode <- input$corpus_timeline_y %||% "corpus"

    # Stable per-corpus colour palette.
    corpora_names <- unique(df$corpus)
    base_pal <- c("#377EB8","#E41A1C","#4DAF4A","#984EA3","#FF7F00",
                  "#A65628","#F781BF","#66C2A5","#FFD92F","#8DA0CB")
    pal <- setNames(rep(base_pal, length.out = length(corpora_names)),
                    corpora_names)

    df$y_label <- if (y_mode == "corpus") df$corpus else df$article
    # Order Y axis by earliest creation per group, oldest at top.
    y_order <- df %>%
      dplyr::group_by(y_label) %>%
      dplyr::summarise(min_ts = min(created_at, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(.data$min_ts) %>%
      dplyr::pull(y_label)
    df$y_label <- factor(df$y_label, levels = rev(y_order))

    fmt_int <- function(x) ifelse(is.na(x), "—", format(x, big.mark = ","))
    df$hover <- sprintf(
      paste0("<b>%s</b><br>",
             "Corpus: %s<br>",
             "Created: %s by %s<br>",
             "Revisions: %s · Editors: %s<br>",
             "Pageviews (30d): %s · Assessment: %s"),
      htmltools::htmlEscape(df$article),
      htmltools::htmlEscape(df$corpus),
      format(df$created_at, "%Y-%m-%d"),
      htmltools::htmlEscape(df$creator %||% "?"),
      fmt_int(df$n_revisions),
      fmt_int(df$n_editors),
      fmt_int(df$pageviews),
      ifelse(is.na(df$assessment), "—", htmltools::htmlEscape(df$assessment))
    )

    df$wp_url <- sprintf(
      "https://%s.wikipedia.org/wiki/%s",
      d$lang,
      vapply(df$article,
             function(t) utils::URLencode(gsub(" ", "_", t),
                                          reserved = FALSE),
             character(1L))
    )
    df$hover <- paste0(df$hover,
      '<br><i style="color:#888;">click to open Wikipedia ↗</i>')

    fig <- plotly::plot_ly()
    for (cn in corpora_names) {
      sub <- df[df$corpus == cn, , drop = FALSE]
      if (nrow(sub) == 0L) next
      sizes <- if (all(is.na(sub$n_revisions))) 10
               else pmin(28, pmax(7, sqrt(pmax(sub$n_revisions, 1, na.rm = TRUE)) * 1.6))
      fig <- fig %>% plotly::add_markers(
        x = sub$created_at, y = sub$y_label,
        name = cn,
        marker = list(color = pal[[cn]], size = sizes,
                      line = list(width = 1, color = "#fff"),
                      opacity = 0.85),
        text = sub$hover, hoverinfo = "text",
        customdata = sub$wp_url,
        legendgroup = cn
      )
    }

    fig <- fig %>% plotly::layout(
      title = list(text = sprintf(
        "<b>Corpus timeline — article creation dates</b><br><sup>%d articles · %d corpora · marker size ∝ √(revisions)</sup>",
        nrow(df), length(corpora_names)),
        font = list(size = 13)),
      xaxis = list(title = "Article creation date", type = "date"),
      yaxis = list(title = "", automargin = TRUE,
                   tickfont = list(size = 10)),
      legend = list(orientation = "h", x = 0, y = -0.12,
                    title = list(text = "Corpus")),
      margin = list(l = 200, r = 30, t = 60, b = 60)
    )

    # Open the Wikipedia article in a new tab on point click.
    htmlwidgets::onRender(fig, "
      function(el, x) {
        el.on('plotly_click', function(d) {
          if (!d || !d.points || d.points.length === 0) return;
          var url = d.points[0].customdata;
          if (url) window.open(url, '_blank', 'noopener');
        });
      }
    ")
  })

  if (has_vis) {
    output$corpus_cocite <- renderVisNetwork({
      d  <- corpus_data(); req(!is.null(d))
      df <- corpus_doi_df_rv()
      if (is.null(df) || nrow(df) == 0L) {
        showNotification("No DOIs found in any corpus article.",
                         type = "warning")
        return(NULL)
      }

      # Pair counts: how many DOIs each article pair shares.
      df_split <- split(df$doi, df$article)
      arts <- names(df_split)
      if (length(arts) < 2L) return(NULL)

      pair_rows <- list()
      for (i in seq_len(length(arts) - 1L)) {
        for (j in seq.int(i + 1L, length(arts))) {
          shared <- length(intersect(df_split[[i]], df_split[[j]]))
          if (shared >= 1L) {
            pair_rows[[length(pair_rows) + 1L]] <- data.frame(
              from = arts[i], to = arts[j],
              shared = shared, stringsAsFactors = FALSE
            )
          }
        }
      }
      if (length(pair_rows) == 0L) {
        showNotification("No article pairs share at least one DOI.",
                         type = "warning")
        return(NULL)
      }
      edges <- do.call(rbind, pair_rows)

      # Map article → its corpus (first occurrence wins for duplicates).
      corp_map <- df[!duplicated(df$article), c("article", "corpus")]
      corpora_names <- unique(corp_map$corpus)
      base_pal <- c("#377EB8","#E41A1C","#4DAF4A","#984EA3","#FF7F00",
                    "#A65628","#F781BF","#66C2A5","#FFD92F","#8DA0CB")
      pal <- setNames(rep(base_pal, length.out = length(corpora_names)),
                      corpora_names)

      # Node size: # DOIs in each article.
      doi_count <- vapply(df_split, length, integer(1L))
      node_arts <- intersect(arts,
                             unique(c(edges$from, edges$to)))  # connected only
      nodes <- data.frame(
        id    = node_arts,
        label = node_arts,
        title = sprintf("<b>%s</b><br>Corpus: %s<br>DOIs: %d",
                        node_arts,
                        corp_map$corpus[match(node_arts, corp_map$article)],
                        doi_count[node_arts]),
        value = doi_count[node_arts],
        group = corp_map$corpus[match(node_arts, corp_map$article)],
        stringsAsFactors = FALSE
      )
      edges$value <- edges$shared
      edges$title <- sprintf("%d shared DOIs", edges$shared)

      # Pre-build node URLs so clicks open the matching Wikipedia article
      # in a new tab without an extra round-trip.
      nodes$url <- sprintf(
        "https://%s.wikipedia.org/wiki/%s",
        d$lang,
        vapply(node_arts,
               function(t) utils::URLencode(gsub(" ", "_", t),
                                            reserved = FALSE),
               character(1L))
      )
      # Add a hint to the tooltip so users know nodes are clickable.
      nodes$title <- paste0(
        nodes$title,
        '<br><i style="color:#888;">click to open Wikipedia ↗</i>'
      )

      open_wp_js <- "function(props) {
        if (!props || !props.nodes || props.nodes.length === 0) return;
        var nid = props.nodes[0];
        var node = this.body.data.nodes.get(nid);
        if (node && node.url) window.open(node.url, '_blank', 'noopener');
      }"

      vn <- visNetwork::visNetwork(
        nodes, edges, height = "600px", width = "100%",
        main = list(text = sprintf(
          "Article co-citation — %d articles · %d edges",
          nrow(nodes), nrow(edges)),
          style = "font-size:14px; font-weight:600; text-align:center;"),
        submain = list(text = "Click a node to open the Wikipedia article",
                       style = "font-size:11px; color:#777; text-align:center;")
      ) %>%
        visNetwork::visEdges(color = list(color = "#bbbbbb",
                                          highlight = "#e07b00"),
                             smooth = FALSE,
                             scaling = list(min = 1, max = 10)) %>%
        visNetwork::visNodes(scaling = list(min = 12, max = 50,
                                            label = list(enabled = TRUE,
                                                         min = 11, max = 18)),
                             font = list(size = 14)) %>%
        visNetwork::visPhysics(solver = "forceAtlas2Based",
                               forceAtlas2Based = list(gravitationalConstant = -45,
                                                       avoidOverlap = 0.3),
                               stabilization = list(enabled = TRUE,
                                                    iterations = 150)) %>%
        visNetwork::visOptions(highlightNearest = list(enabled = TRUE,
                                                       degree = 1, hover = TRUE),
                               nodesIdSelection = list(enabled = TRUE,
                                                       useLabels = TRUE)) %>%
        visNetwork::visInteraction(navigationButtons = TRUE,
                                   tooltipDelay = 100) %>%
        visNetwork::visEvents(
          selectNode = open_wp_js,
          stabilizationIterationsDone = "function() { this.setOptions({physics: false}); }"
        )

      # Apply per-corpus colour groups.
      for (cn in corpora_names) {
        vn <- visNetwork::visGroups(
          vn, groupname = cn,
          color = list(background = pal[[cn]],
                       border = "#333",
                       highlight = list(background = pal[[cn]],
                                        border = "#000"))
        )
      }
      visNetwork::visLegend(vn, useGroups = TRUE, width = 0.18,
                            position = "right", main = "Corpus")
    })

    output$corpus_pubnet <- renderVisNetwork({
      d  <- corpus_data(); req(!is.null(d))
      df <- corpus_doi_df_rv()
      if (is.null(df) || nrow(df) == 0L) {
        showNotification("No DOIs found in any corpus article.",
                         type = "warning")
        return(NULL)
      }

      # Bipartite graph: Article ↔ Publication (DOI). Keep only DOIs cited
      # by at least min_wiki_count articles, and the top_n_dois most-cited.
      min_wiki  <- max(1L, as.integer(input$pubnet_min_wiki  %||% 2L))
      top_n     <- max(5L, as.integer(input$pubnet_top_n     %||% 50L))

      doi_counts <- df %>%
        dplyr::distinct(article, doi) %>%
        dplyr::group_by(doi) %>%
        dplyr::summarise(n_wiki = dplyr::n(), .groups = "drop") %>%
        dplyr::filter(n_wiki >= min_wiki) %>%
        dplyr::arrange(dplyr::desc(n_wiki)) %>%
        head(top_n)

      if (nrow(doi_counts) == 0L) {
        showNotification(sprintf(
          "No DOIs are cited by ≥ %d articles. Lower the threshold.",
          min_wiki), type = "warning")
        return(NULL)
      }

      df_keep <- df %>% dplyr::filter(doi %in% doi_counts$doi)
      art_in_net <- unique(df_keep$article)
      if (length(art_in_net) == 0L) return(NULL)

      # Map article → corpus (first occurrence wins).
      corp_map <- df[!duplicated(df$article), c("article", "corpus")]
      corpora_names <- unique(corp_map$corpus)
      base_pal <- c("#377EB8","#E41A1C","#4DAF4A","#984EA3","#FF7F00",
                    "#A65628","#F781BF","#66C2A5","#FFD92F","#8DA0CB")
      pal <- setNames(rep(base_pal, length.out = length(corpora_names)),
                      corpora_names)

      # ── Article nodes ────────────────────────────────────────────────
      art_doi_count <- df_keep %>%
        dplyr::distinct(article, doi) %>%
        dplyr::group_by(article) %>%
        dplyr::summarise(n_doi = dplyr::n(), .groups = "drop")

      art_url <- sprintf(
        "https://%s.wikipedia.org/wiki/%s",
        d$lang,
        vapply(art_in_net,
               function(t) utils::URLencode(gsub(" ", "_", t),
                                            reserved = FALSE),
               character(1L))
      )

      art_nodes <- data.frame(
        id    = paste0("A::", art_in_net),
        label = art_in_net,
        title = sprintf(
          "<b>%s</b><br>Corpus: %s<br>DOIs in network: %d<br><i style='color:#888'>click to open Wikipedia ↗</i>",
          art_in_net,
          corp_map$corpus[match(art_in_net, corp_map$article)],
          art_doi_count$n_doi[match(art_in_net, art_doi_count$article)]
        ),
        value = art_doi_count$n_doi[match(art_in_net, art_doi_count$article)],
        group = paste0("Article · ",
                       corp_map$corpus[match(art_in_net, corp_map$article)]),
        url   = art_url,
        stringsAsFactors = FALSE
      )

      # ── Publication (DOI) nodes ──────────────────────────────────────
      pub_url <- paste0("https://doi.org/", doi_counts$doi)
      pub_nodes <- data.frame(
        id    = paste0("P::", doi_counts$doi),
        label = doi_counts$doi,
        title = sprintf(
          "<b>%s</b><br>Cited by %d articles in this corpus<br><i style='color:#888'>click to open doi.org ↗</i>",
          doi_counts$doi, doi_counts$n_wiki
        ),
        value = doi_counts$n_wiki,
        group = "Publication",
        url   = pub_url,
        stringsAsFactors = FALSE
      )

      nodes <- rbind(art_nodes, pub_nodes)

      # ── Edges: each (article, doi) pair becomes an edge ──────────────
      edges <- df_keep %>%
        dplyr::distinct(article, doi) %>%
        dplyr::transmute(
          from = paste0("A::", article),
          to   = paste0("P::", doi),
          value = 1L,
          title = paste0(article, " → ", doi)
        ) %>%
        as.data.frame()

      open_url_js <- "function(props) {
        if (!props || !props.nodes || props.nodes.length === 0) return;
        var nid = props.nodes[0];
        var node = this.body.data.nodes.get(nid);
        if (node && node.url) window.open(node.url, '_blank', 'noopener');
      }"

      vn <- visNetwork::visNetwork(
        nodes, edges, height = "600px", width = "100%",
        main = list(
          text = sprintf(
            "Article ↔ Publication network — %d articles · %d publications · %d edges",
            length(art_in_net), nrow(pub_nodes), nrow(edges)),
          style = "font-size:14px; font-weight:600; text-align:center;"),
        submain = list(
          text = sprintf(
            "Click an article to open Wikipedia ↗ · click a publication to open doi.org ↗ · min_wiki=%d, top_n=%d",
            min_wiki, top_n),
          style = "font-size:11px; color:#777; text-align:center;")
      ) %>%
        visNetwork::visEdges(color = list(color = "#cccccc",
                                          highlight = "#F28E2B"),
                             smooth = list(enabled = TRUE,
                                           type = "curvedCW",
                                           roundness = 0.2),
                             arrows = list(to = list(enabled = TRUE,
                                                     scaleFactor = 0.4))) %>%
        visNetwork::visNodes(scaling = list(min = 10, max = 50,
                                            label = list(enabled = TRUE,
                                                         min = 10, max = 22))) %>%
        visNetwork::visPhysics(solver = "forceAtlas2Based",
                               forceAtlas2Based = list(
                                 gravitationalConstant = -60,
                                 centralGravity = 0.005,
                                 springLength = 100,
                                 springConstant = 0.08,
                                 avoidOverlap = 0.3),
                               stabilization = list(enabled = TRUE,
                                                    iterations = 150)) %>%
        visNetwork::visOptions(highlightNearest = list(enabled = TRUE,
                                                       degree = 1, hover = TRUE),
                               nodesIdSelection = list(enabled = TRUE,
                                                       useLabels = TRUE)) %>%
        visNetwork::visInteraction(navigationButtons = TRUE,
                                   tooltipDelay = 100) %>%
        visNetwork::visEvents(
          selectNode = open_url_js,
          stabilizationIterationsDone = "function() { this.setOptions({physics: false}); }"
        )

      # Per-corpus article colour group + a fixed colour for publications.
      for (cn in corpora_names) {
        vn <- visNetwork::visGroups(
          vn, groupname = paste0("Article · ", cn),
          shape = "box", font = list(color = "white", size = 13),
          color = list(background = pal[[cn]], border = "#333",
                       highlight = list(background = pal[[cn]], border = "#000"))
        )
      }
      vn <- visNetwork::visGroups(
        vn, groupname = "Publication", shape = "dot",
        color = list(background = "#F28E2B", border = "#a05e0f",
                     highlight = list(background = "#ffb55f",
                                      border = "#a05e0f"))
      )

      visNetwork::visLegend(vn, useGroups = TRUE, width = 0.22,
                            position = "right", main = "Legend")
    })

    output$corpus_wikilink <- renderVisNetwork({
      d  <- corpus_data(); req(!is.null(d))
      df <- corpus_wikilink_df_rv()
      if (is.null(df) || nrow(df) == 0L) {
        showNotification("No wikilinks extracted from corpus articles.",
                         type = "warning")
        return(NULL)
      }

      min_src   <- max(1L, as.integer(input$wikilink_min_sources %||% 2L))
      top_n     <- max(5L, as.integer(input$wikilink_top_n        %||% 75L))
      corpus_only <- isTRUE(input$wikilink_corpus_only)

      corpus_articles <- unique(df$article)

      # Optionally restrict edges to corpus-internal targets only.
      df_work <- if (corpus_only) {
        df[df$target %in% corpus_articles, , drop = FALSE]
      } else df

      if (nrow(df_work) == 0L) {
        showNotification(
          "No corpus-internal links found. Untick 'corpus only' to widen.",
          type = "warning")
        return(NULL)
      }

      tgt_counts <- df_work %>%
        dplyr::distinct(article, target) %>%
        dplyr::group_by(target) %>%
        dplyr::summarise(n_src = dplyr::n(), .groups = "drop") %>%
        dplyr::filter(n_src >= min_src) %>%
        dplyr::arrange(dplyr::desc(n_src)) %>%
        head(top_n)

      if (nrow(tgt_counts) == 0L) {
        showNotification(sprintf(
          "No targets are linked from ≥ %d articles. Lower the threshold.",
          min_src), type = "warning")
        return(NULL)
      }

      df_keep <- df_work %>% dplyr::filter(target %in% tgt_counts$target)
      art_in_net <- unique(df_keep$article)
      tgt_in_net <- unique(df_keep$target)
      if (length(art_in_net) == 0L) return(NULL)

      corp_map <- df[!duplicated(df$article), c("article", "corpus")]
      corpora_names <- unique(corp_map$corpus)
      base_pal <- c("#377EB8","#E41A1C","#4DAF4A","#984EA3","#FF7F00",
                    "#A65628","#F781BF","#66C2A5","#FFD92F","#8DA0CB")
      pal <- setNames(rep(base_pal, length.out = length(corpora_names)),
                      corpora_names)

      # ── Source nodes (corpus articles) ───────────────────────────────
      art_link_count <- df_keep %>%
        dplyr::distinct(article, target) %>%
        dplyr::group_by(article) %>%
        dplyr::summarise(n_links = dplyr::n(), .groups = "drop")

      art_url <- sprintf(
        "https://%s.wikipedia.org/wiki/%s",
        d$lang,
        vapply(art_in_net,
               function(t) utils::URLencode(gsub(" ", "_", t),
                                            reserved = FALSE),
               character(1L))
      )

      art_nodes <- data.frame(
        id    = paste0("S::", art_in_net),
        label = art_in_net,
        title = sprintf(
          "<b>%s</b><br>Corpus: %s<br>Outgoing links in network: %d<br><i style='color:#888'>click to open Wikipedia ↗</i>",
          art_in_net,
          corp_map$corpus[match(art_in_net, corp_map$article)],
          art_link_count$n_links[match(art_in_net, art_link_count$article)]),
        value = art_link_count$n_links[match(art_in_net, art_link_count$article)],
        group = paste0("Article · ",
                       corp_map$corpus[match(art_in_net, corp_map$article)]),
        url   = art_url,
        stringsAsFactors = FALSE
      )

      # ── Target nodes ─────────────────────────────────────────────────
      tgt_url <- sprintf(
        "https://%s.wikipedia.org/wiki/%s",
        d$lang,
        vapply(tgt_in_net,
               function(t) utils::URLencode(gsub(" ", "_", t),
                                            reserved = FALSE),
               character(1L))
      )
      tgt_in_corpus <- tgt_in_net %in% corpus_articles
      tgt_group <- ifelse(tgt_in_corpus,
                          "Target (in corpus)",
                          "Target (external)")
      tgt_nodes <- data.frame(
        id    = paste0("T::", tgt_in_net),
        label = tgt_in_net,
        title = sprintf(
          "<b>%s</b><br>Linked from %d corpus article(s)%s<br><i style='color:#888'>click to open Wikipedia ↗</i>",
          tgt_in_net,
          tgt_counts$n_src[match(tgt_in_net, tgt_counts$target)],
          ifelse(tgt_in_corpus, " · in corpus", "")),
        value = tgt_counts$n_src[match(tgt_in_net, tgt_counts$target)],
        group = tgt_group,
        url   = tgt_url,
        stringsAsFactors = FALSE
      )

      # If a target is also a corpus article (S::X exists and T::X exists),
      # collapse so the user sees one node per article.
      collapsed_ids <- intersect(sub("^S::", "", art_nodes$id),
                                 sub("^T::", "", tgt_nodes$id))
      if (length(collapsed_ids) > 0L) {
        # Drop the T:: duplicates and rewrite edges to point to S::.
        tgt_nodes <- tgt_nodes[!sub("^T::", "", tgt_nodes$id) %in% collapsed_ids, , drop = FALSE]
      }

      nodes <- rbind(art_nodes, tgt_nodes)

      edges <- df_keep %>%
        dplyr::distinct(article, target) %>%
        dplyr::transmute(
          from  = paste0("S::", article),
          to    = ifelse(target %in% collapsed_ids,
                         paste0("S::", target),
                         paste0("T::", target)),
          value = 1L,
          title = paste0(article, " → ", target)
        ) %>%
        dplyr::filter(from != to) %>%
        as.data.frame()

      open_url_js <- "function(props) {
        if (!props || !props.nodes || props.nodes.length === 0) return;
        var nid = props.nodes[0];
        var node = this.body.data.nodes.get(nid);
        if (node && node.url) window.open(node.url, '_blank', 'noopener');
      }"

      vn <- visNetwork::visNetwork(
        nodes, edges, height = "600px", width = "100%",
        main = list(
          text = sprintf(
            "Wikilink network — %d corpus articles · %d targets · %d edges",
            length(art_in_net), nrow(tgt_nodes), nrow(edges)),
          style = "font-size:14px; font-weight:600; text-align:center;"),
        submain = list(
          text = sprintf(
            "Click any node to open Wikipedia ↗ · min_src=%d, top_n=%d%s",
            min_src, top_n,
            if (corpus_only) " · corpus-only" else ""),
          style = "font-size:11px; color:#777; text-align:center;")
      ) %>%
        visNetwork::visEdges(color = list(color = "#dddddd",
                                          highlight = "#3878c7"),
                             smooth = list(enabled = TRUE,
                                           type = "curvedCW",
                                           roundness = 0.15),
                             arrows = list(to = list(enabled = TRUE,
                                                     scaleFactor = 0.4))) %>%
        visNetwork::visNodes(scaling = list(min = 10, max = 45,
                                            label = list(enabled = TRUE,
                                                         min = 10, max = 20))) %>%
        visNetwork::visPhysics(solver = "forceAtlas2Based",
                               forceAtlas2Based = list(
                                 gravitationalConstant = -55,
                                 centralGravity = 0.005,
                                 springLength = 110,
                                 springConstant = 0.07,
                                 avoidOverlap = 0.4),
                               stabilization = list(enabled = TRUE,
                                                    iterations = 150)) %>%
        visNetwork::visOptions(highlightNearest = list(enabled = TRUE,
                                                       degree = 1, hover = TRUE),
                               nodesIdSelection = list(enabled = TRUE,
                                                       useLabels = TRUE)) %>%
        visNetwork::visInteraction(navigationButtons = TRUE,
                                   tooltipDelay = 100) %>%
        visNetwork::visEvents(
          selectNode = open_url_js,
          stabilizationIterationsDone = "function() { this.setOptions({physics: false}); }"
        )

      for (cn in corpora_names) {
        vn <- visNetwork::visGroups(
          vn, groupname = paste0("Article · ", cn),
          shape = "box", font = list(color = "white", size = 13),
          color = list(background = pal[[cn]], border = "#333",
                       highlight = list(background = pal[[cn]], border = "#000"))
        )
      }
      vn <- visNetwork::visGroups(
        vn, groupname = "Target (in corpus)",
        shape = "diamond",
        color = list(background = "#3878c7", border = "#1f4d80",
                     highlight = list(background = "#5b9be0",
                                      border = "#1f4d80")),
        font = list(color = "white", size = 12)
      )
      vn <- visNetwork::visGroups(
        vn, groupname = "Target (external)",
        shape = "dot",
        color = list(background = "#bdbdbd", border = "#666",
                     highlight = list(background = "#dadada", border = "#333")),
        font = list(size = 11)
      )

      visNetwork::visLegend(vn, useGroups = TRUE, width = 0.22,
                            position = "right", main = "Legend")
    })
  }
}

# ---------------------------------------------------------------------------
shinyApp(ui, server)
