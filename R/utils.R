# utils.R — shared helpers for history-flow analysis

#' Parse MediaWiki ISO timestamps to POSIXct (UTC)
#'
#' @param x Character vector of timestamps in `\%Y-\%m-\%dT\%H:\%M:\%SZ` format.
#' @return POSIXct vector (UTC timezone).
#' @export
parse_ts <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Return TRUE for IP-address editors (IPv4 or IPv6)
#'
#' @param user Character vector of editor names.
#' @return Logical vector.
#' @export
is_anonymous <- function(user) {
  ipv4 <- grepl("^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$", user)
  ipv6 <- grepl("^[0-9a-fA-F]{1,4}(:[0-9a-fA-F]{0,4}){2,7}$", user, perl = TRUE)
  ipv4 | ipv6
}

#' Collapse users into top-N + "Anonymous" + "Other (registered)"
#'
#' @param user    Character vector of editor names.
#' @param is_anon Logical vector (TRUE = anonymous IP).
#' @param n_top   Number of top registered editors to label individually.
#' @return Character vector of author labels.
#' @export
label_authors <- function(user, is_anon, n_top = 12) {
  top <- names(sort(table(user[!is_anon]), decreasing = TRUE))[seq_len(n_top)]
  dplyr::case_when(
    is_anon       ~ "Anonymous",
    user %in% top ~ user,
    TRUE          ~ "Other (registered)"
  )
}

#' Return TRUE for edits that look like reverts
#'
#' Uses the `tags` column (pipe-separated) and edit summary fallback.
#'
#' @param tags    Character vector of MediaWiki revision tags.
#' @param comment Character vector of edit summaries.
#' @return Logical vector.
#' @export
is_revert_edit <- function(tags, comment) {
  tags    <- ifelse(is.na(tags),    "", tags)
  comment <- ifelse(is.na(comment), "", comment)
  tag_hit <- grepl("mw-reverted|mw-undo|mw-rollback", tags,    ignore.case = TRUE)
  cmt_hit <- grepl(
    "^(revert|rv[^e]|reverted|undid revision|undo\\b|rollback)",
    trimws(comment), ignore.case = TRUE, perl = TRUE
  )
  tag_hit | cmt_hit
}

#' Named colour vector for a set of author labels
#'
#' "Other (registered)" and "Anonymous" always get fixed neutral colours.
#'
#' @param labels Character vector of author labels.
#' @return Named character vector of hex colours.
#' @export
author_palette <- function(labels) {
  named <- setdiff(unique(labels), c("Anonymous", "Other (registered)"))
  base  <- c(
    "#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
    "#A65628","#F781BF","#66C2A5","#FC8D62","#8DA0CB",
    "#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3"
  )
  pal <- setNames(base[seq_along(named)], named)
  pal["Anonymous"]           <- "#999999"
  pal["Other (registered)"] <- "#DDDDDD"
  pal
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

minutes_between <- function(t1, t2) as.numeric(difftime(t2, t1, units = "mins"))
