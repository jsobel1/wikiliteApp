# =============================================================================
# Helpers shared between the Shiny app and the package's test suite.
# These functions are exported so they can be exercised by testthat without
# having to source(inst/shiny/app.R), which has heavy UI side-effects.
# =============================================================================

#' Sample the rows of a chronologically ordered revision history.
#'
#' Used by the Shiny app to keep large histories tractable. The returned
#' data frame is always still in chronological order; the first and last
#' rows are preserved by `even` and `random` so the timeline endpoints
#' don't shift.
#'
#' @param h Data frame of revisions, sorted oldest → newest.
#' @param n Target number of revisions to retain.
#' @param method One of `"recent"` (keep the last n rows), `"even"`
#'   (evenly spaced across the range, retaining first and last), or
#'   `"random"` (random subset, retaining first and last).
#' @return A sub-sampled data frame in chronological order. If
#'   `nrow(h) <= n`, the input is returned unchanged.
#' @export
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

#' Convert a `#RRGGBB` hex colour to an `rgba(r,g,b,a)` string.
#' @param hex Character vector of `#RRGGBB` colours (length 1).
#' @param alpha Alpha in `[0, 1]`.
#' @return Character scalar of the form `"rgba(r,g,b,a)"`.
#' @export
hex_to_rgba <- function(hex, alpha = 0.82) {
  hex <- sub("^#", "", hex)
  sprintf("rgba(%d,%d,%d,%.2f)",
          strtoi(substr(hex, 1, 2), 16L),
          strtoi(substr(hex, 3, 4), 16L),
          strtoi(substr(hex, 5, 6), 16L),
          alpha)
}

#' Extract `<ref>…</ref>` citation strings from raw wikitext.
#' @param wikitext A character scalar of wikitext.
#' @return Character vector of unique citation strings, whitespace-collapsed.
#' @export
extract_cites <- function(wikitext) {
  if (is.null(wikitext) || !nzchar(wikitext)) return(character(0))
  hits <- regmatches(
    wikitext,
    gregexpr("<ref(?:\\s[^>]*)?>.*?</ref>|<ref\\s[^/]*/?>",
             wikitext, perl = TRUE, ignore.case = TRUE)
  )[[1]]
  unique(trimws(gsub("\\s+", " ", hits)))
}

#' Classify a single citation wikitext string into a reference type.
#' @param x A character scalar (one citation).
#' @return One of "Journal", "Book", "Web", "News/Magazine", "Preprint",
#'   "Thesis", "Conference", "Report", "Multimedia", "Legal/Patent",
#'   "Social Media", "DOI", or "Other".
#' @export
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

#' Extract a canonical URL for a citation.
#'
#' Resolution order: DOI → doi.org link, raw URL → direct, ISBN → WorldCat search.
#' @param x A character scalar (one citation).
#' @return A URL string, or NULL if none could be resolved.
#' @export
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

#' Shorten a raw citation string to a human-readable display label.
#' @param x A character scalar (one citation).
#' @return A short label: DOI if present, otherwise "Last (Year)" if a
#'   `|last=` field is present, otherwise the first 70 characters.
#' @export
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

#' Extract a normalised ISBN from a citation wikitext string.
#'
#' Looks for an `isbn` template field, strips dashes/spaces, and returns
#' the digits-only (or X-terminated) form. Returns `NA_character_` if no
#' valid ISBN of length >= 10 is found.
#'
#' @param x A character scalar (one citation).
#' @return A character scalar (the normalised ISBN), or `NA_character_`.
#' @export
extract_isbn <- function(x) {
  m <- regmatches(x, regexpr("(?i)isbn\\s*[=:]?\\s*([0-9][0-9X\\- ]{8,})",
                             x, perl = TRUE))
  if (!length(m) || !nzchar(m[[1]])) return(NA_character_)
  raw <- sub("(?i)isbn[^0-9]*", "", m[[1]], perl = TRUE)
  iso <- gsub("[^0-9X]", "", toupper(raw))
  if (nchar(iso) >= 10L) iso else NA_character_
}

#' Compute a set-based line-level diff between two wikitext strings.
#'
#' Lines that are blank or whitespace-only are ignored, since those would
#' otherwise dominate any diff. The diff is set-based (a line that moves
#' position is unchanged); this is the same approximation the Shiny diff
#' panel uses.
#'
#' @param old_text,new_text Character scalars (or `NULL`).
#' @return A list with `removed`, `added`, `n_removed`, `n_added`,
#'   `n_unchanged`, or `NULL` if either input is `NULL`.
#' @export
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
