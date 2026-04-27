# =============================================================================
# Wikipedia History Flow — interactive Shiny app
#
# Replicates the Microsoft Wikipedia History Flow Tool (Viégas et al., CHI 2004)
#
# Layout
#   [Controls]  Article | From | To | Min Δ bytes | Spacing | Fetch
#   [Left]      Author list (colour-coded, edit counts)
#   [Centre]    Tabs: History Flow | Citation Analysis
#   [Right]     Revision selector, wikitext (coloured by author), diff panel
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

#' Fetch raw wikitext for a specific revision ID from the MediaWiki API
fetch_wikitext_for_revid <- function(revid, lang = "en") {
  url <- sprintf(
    paste0("https://%s.wikipedia.org/w/api.php?action=query",
           "&prop=revisions&revids=%s&rvprop=content&rvslots=main",
           "&format=json&formatversion=2"),
    lang, revid
  )
  old_to <- getOption("timeout")
  options(timeout = 12)
  on.exit(options(timeout = old_to), add = TRUE)
  resp <- tryCatch(
    jsonlite::fromJSON(url, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
  tryCatch(
    resp$query$pages[[1]]$revisions[[1]]$slots$main$content,
    error = function(e) NULL
  )
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
  # CS1/CS2 template-based checks — most specific first
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
  # Standalone {{doi}} template (bare DOI without a recognized cite template)
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
#' Returns list(removed, added, n_removed, n_added, n_unchanged) or
#' list(first_rev = TRUE) when there is no previous revision.
compute_line_diff <- function(old_text, new_text) {
  if (is.null(old_text) || is.null(new_text)) return(NULL)

  # Split and strip blank lines for a meaningful diff
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
.ctrl-bar { background:#f0f2f5; padding:10px 16px 6px; border-bottom:1px solid #d0d3d8; margin-bottom:10px; }
.panel-hdr { font-size:12.5px; font-weight:600; color:#444; margin:0 0 4px 0; }
.author-chip {
  display:flex; align-items:center; gap:5px;
  padding:3px 5px; margin-bottom:2px; border-radius:3px;
  font-size:11.5px; line-height:1.3;
}
.author-chip:hover { background:#eef; }
.swatch { width:11px; height:11px; border-radius:2px; flex-shrink:0; }
.aname  { flex:1; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.acount { color:#888; font-size:10px; white-space:nowrap; min-width:22px; text-align:right; }
.rev-meta { font-size:11px; color:#666; margin:2px 0 5px 0; min-height:1.4em; }
.wikibox {
  height:190px; overflow-y:auto; font-size:11px; font-family:'Consolas',monospace;
  white-space:pre-wrap; word-break:break-word;
  background:#fafafa; border:1px solid #ddd; padding:8px 10px;
  border-radius:3px; line-height:1.5;
}
.wikibox mark { background:#fffacd; border-radius:2px; }
.diff-box {
  height:190px; overflow-y:auto; font-size:11px; font-family:'Consolas',monospace;
  white-space:pre-wrap; word-break:break-word;
  background:#fafafa; border:1px solid #ddd; padding:8px 10px;
  border-radius:3px; line-height:1.5;
}
.attr-table { width:100%; border-collapse:collapse; font-size:10.5px; margin-top:6px; }
.attr-table th { background:#f0f2f5; padding:3px 5px; text-align:left; font-weight:600; border-bottom:1px solid #ddd; }
.attr-table td { padding:2px 5px; border-bottom:1px solid #f0f0f0; vertical-align:middle; }
.attr-swatch { display:inline-block; width:10px; height:10px; border-radius:2px; margin-right:4px; vertical-align:middle; }
.diff-added   { display:block; background:#e6ffed; color:#22863a; padding:0 4px; }
.diff-removed { display:block; background:#ffeef0; color:#b31d28; padding:0 4px; }
.diff-note    { color:#999; font-size:10px; font-style:italic; padding:2px 4px; }
input[type='color'].color-pick {
  width:14px; height:14px; border:none; padding:0; cursor:pointer;
  border-radius:2px; flex-shrink:0; vertical-align:middle;
  -webkit-appearance:none;
}
input[type='color'].color-pick::-webkit-color-swatch-wrapper { padding:0; }
input[type='color'].color-pick::-webkit-color-swatch { border:none; border-radius:2px; }
.grp-chip {
  display:flex; align-items:center; gap:4px;
  padding:2px 4px; margin-bottom:2px;
  border-radius:3px; font-size:11.5px;
}
.grp-chip:hover { background:#eef; }
.grp-chip.disabled { opacity:0.4; }
.metric-row { display:flex; flex-wrap:wrap; gap:8px; margin-bottom:10px; }
.metric-card {
  display:flex; flex-direction:column; align-items:center;
  background:#f8f9fa; border:1px solid #e9ecef; border-radius:6px;
  padding:7px 14px; min-width:90px;
}
.metric-val { font-size:20px; font-weight:700; color:#2c3e50; line-height:1.2; }
.metric-lbl { font-size:9.5px; color:#888; margin-top:2px; text-transform:uppercase; letter-spacing:0.04em; }
.wiki-art-link { font-size:12px; margin:0 0 4px 2px; }
.wiki-art-link a { color:#0645ad; text-decoration:none; }
.wiki-art-link a:hover { text-decoration:underline; }
.dl-bar { display:flex; align-items:center; gap:6px; margin-bottom:6px; }
.sci-bar-wrap { height:6px; background:#e8e8e8; border-radius:3px; margin-top:4px; width:100%; }
.sci-bar-fill { height:6px; border-radius:3px; }
.latency-section { margin-top:10px; padding-top:8px; border-top:1px solid #e8e8e8; }
"

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  tags$title("Wikipedia History Flow"),

  # ---- Controls bar --------------------------------------------------------
  div(class = "ctrl-bar",
    fluidRow(
      column(3,
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
                  data: {
                    action: 'opensearch',
                    search: query,
                    limit: 12,
                    namespace: 0,
                    format: 'json',
                    origin: '*'
                  },
                  type: 'GET',
                  dataType: 'json',
                  error: function() { callback(); },
                  success: function(res) {
                    callback(res[1].map(function(t) {
                      return { value: t, label: t };
                    }));
                  }
                });
              }")
            )
          )
        )
      ),
      column(2,
        dateInput("date_from", "From", value = "2001-01-01",
                  min = "2001-01-01", width = "100%")
      ),
      column(2,
        dateInput("date_to", "To", value = Sys.Date(), width = "100%")
      ),
      column(2,
        numericInput("min_delta", "Min |Δ| bytes",
                     value = 0, min = 0, max = 100000, step = 50, width = "100%")
      ),
      column(2,
        fluidRow(
          column(7,
            selectInput("spacing", "Spacing",
                        choices = c("revision", "date"),
                        selected = "revision", width = "100%")
          ),
          column(5,
            numericInput("n_top", "Top N",
                         value = 8, min = 4, max = 15, step = 1, width = "100%")
          )
        )
      ),
      column(1,
        br(),
        actionButton("go", "Fetch →",
                     class = "btn-primary",
                     style = "width:100%; padding:5px 4px;")
      )
    )
  ),

  # ---- Main 3-panel layout -------------------------------------------------
  fluidRow(

    # Left: author list
    column(2,
      p(class = "panel-hdr", "Authors"),
      div(style = "height:570px; overflow-y:auto;",
          uiOutput("author_list"))
    ),

    # Centre: tabbed visualisation
    column(6,
      uiOutput("wiki_link_ui"),
      tabsetPanel(id = "main_tabs", type = "tabs",

        tabPanel("History Flow",
          br(),
          echarts4rOutput("hf_plot", height = "500px", width = "100%")
        ),

        # ── Citation Analysis ──────────────────────────────────────────────
        tabPanel("Citation Analysis",
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
              actionButton("analyse_citations", "Analyse Citations",
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
            column(12,
              actionButton("annotate_epmc", "Annotate with EuropePMC",
                           class = "btn-sm btn-default",
                           style = "margin-bottom:6px;")
            )
          ),
          uiOutput("sci_cards"),
          div(style = "height:380px; overflow-y:auto; border:1px solid #e8e8e8; border-radius:3px;",
              plotlyOutput("cite_plot", height = "auto")),
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

        # ── Authorship ────────────────────────────────────────────────────
        tabPanel("Authorship",
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

        # ── Stability ─────────────────────────────────────────────────────
        tabPanel("Stability",
          br(),
          uiOutput("stab_cards"),
          plotlyOutput("stab_size_plot",  height = "270px"),
          fluidRow(
            column(6, plotOutput("stab_delta_plot",  height = "240px")),
            column(6, plotOutput("stab_rhythm_plot", height = "240px"))
          ),
          br(),
          div(class = "dl-bar",
              if (has_xlsx)
                downloadButton("dl_history_xlsx", "⬇ Revision history XLSX",
                               class = "btn-sm btn-default")
          )
        ),

        # ── Vandalism & Edit Wars ─────────────────────────────────────────
        tabPanel("Vandalism & Wars",
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

        # ── Multi-Article ──────────────────────────────────────────────────
        tabPanel("Multi-Article",
          br(),
          fluidRow(
            column(8,
              textAreaInput("multi_articles",
                            "Article titles — one per line (min 2)",
                            rows = 4, width = "100%",
                            placeholder = "Zeitgeber\nCircadian rhythm\nSleep deprivation")
            ),
            column(2,
              selectInput("multi_lang", "Language",
                          choices = c("en","fr","de","es","it","pt","nl","ru","ja","zh"),
                          selected = "en", width = "100%"),
              selectInput("multi_color_by", "Timeline colour",
                          choices = c("sciscore", "size", "none"),
                          selected = "sciscore", width = "100%")
            ),
            column(2,
              br(), br(),
              actionButton("multi_fetch", "Fetch →",
                           class = "btn-primary", style = "width:100%;")
            )
          ),
          tabsetPanel(id = "multi_subtabs",
            tabPanel("Timeline",
              br(),
              plotlyOutput("multi_timeline", height = "520px")
            ),
            tabPanel("Co-citation Network",
              br(),
              if (has_vis)
                visNetworkOutput("multi_cocite", height = "600px")
              else
                div(class = "latency-section",
                    p("Install the ", tags$code("visNetwork"), " package for network plots."))
            ),
            tabPanel("Publication Network",
              br(),
              if (has_vis)
                visNetworkOutput("multi_pubnet", height = "600px")
              else
                div(class = "latency-section",
                    p("Install the ", tags$code("visNetwork"), " package for network plots."))
            ),
            tabPanel("Wikilink Network",
              br(),
              if (has_vis)
                visNetworkOutput("multi_wikilink", height = "600px")
              else
                div(class = "latency-section",
                    p("Install the ", tags$code("visNetwork"), " package for network plots."))
            )
          )
        ),

        # ── Category Explorer ──────────────────────────────────────────────
        tabPanel("Category Explorer",
          br(),
          fluidRow(
            column(6,
              textInput("cat_name", "Wikipedia Category",
                        placeholder = "e.g. Circadian rhythm", width = "100%")
            ),
            column(3,
              selectInput("cat_lang", "Language",
                          choices = c("en","fr","de","es","it","pt","nl","ru"),
                          selected = "en", width = "100%")
            ),
            column(3,
              br(),
              actionButton("cat_fetch", "Browse Category",
                           class = "btn-info", style = "width:100%;")
            )
          ),
          uiOutput("cat_info"),
          br(),
          fluidRow(
            column(6,
              tags$b("Articles in category"),
              uiOutput("cat_articles_ui"),
              br(),
              actionButton("cat_send_multi", "→ Send to Multi-Article tab",
                           class = "btn-sm btn-default")
            ),
            column(6,
              tags$b("Subcategories"),
              br(),
              if (has_DT)
                DT::DTOutput("cat_subcat_tbl")
              else
                tableOutput("cat_subcat_base")
            )
          )
        )

      )
    ),

    # Right: revision selector + wikitext + diff
    column(4,
      # Revision selector
      p(class = "panel-hdr", "Revision"),
      selectInput("rev_selector", label = NULL,
                  choices  = character(0),
                  width    = "100%"),
      uiOutput("rev_meta"),

      # Authored text
      p(class = "panel-hdr", "Text (coloured by author)"),
      uiOutput("wikitext_panel"),

      # Diff with previous revision
      p(class = "panel-hdr", style = "margin-top:8px;",
        "Changes vs. previous revision"),
      uiOutput("diff_panel")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # Maximum revisions to hold in memory after date-filtering.
  # Keeps prepare_history() fast for articles with 100 K+ revisions.
  N_MAX_REVISIONS <- 5000L

  # ---- Fetch raw history on button click -----------------------------------

  raw_hist <- eventReactive(input$go, {
    article <- trimws(input$article)
    shiny::validate(shiny::need(nzchar(article), "Please enter an article name."))
    shiny::validate(shiny::need(nchar(article) <= 255, "Article name is too long."))
    shiny::validate(shiny::need(
      !grepl("[<>{}|\\[\\]\n\r\t]", article),
      "Article name contains invalid characters."
    ))

    withProgress(message = paste0("Fetching '", article, "' …"), value = 0.2, {
      raw <- tryCatch(
        fetch_history(
          article,
          date_an   = format(as.POSIXct(input$date_to, tz = "UTC"),
                             "%Y-%m-%dT%H:%M:%SZ"),
          cache_dir = normalizePath("../cache")
        ),
        error = function(e) {
          showNotification(paste("Fetch error:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        }
      )
      req(!is.null(raw))

      n_raw <- nrow(raw)
      if (n_raw > N_MAX_REVISIONS) {
        showNotification(
          sprintf("'%s' has %s revisions — analysis capped at the most recent %s within the date range.",
                  article,
                  format(n_raw, big.mark = ","),
                  format(N_MAX_REVISIONS, big.mark = ",")),
          type = "warning", duration = 10
        )
      }

      incProgress(0.4, detail = sprintf("Preparing %s revisions…",
                                        format(n_raw, big.mark = ",")))
      # Run the expensive base preparation once, gated on Fetch.
      # n_top=15 (UI max) is used here; author_label is re-applied cheaply
      # in hist_data() after date-filtering, so the top-N set is always
      # computed from the visible date range rather than the full history.
      h <- prepare_history(raw, n_top = 15L)
      if (!"art" %in% names(h)) h$art <- article
      h
    })
  })

  # ---- Filter + relabel on the already-prepared data (fast) ----------------
  # This reactive re-runs on date/n_top/min_delta changes without re-fetching
  # or re-running the expensive base preparation.

  hist_data <- reactive({
    req(raw_hist())
    h <- raw_hist()   # already prepared (see above)

    d_from <- as.POSIXct(input$date_from, tz = "UTC")
    d_to   <- as.POSIXct(input$date_to,   tz = "UTC")
    h      <- filter(h, ts_posix >= d_from, ts_posix <= d_to)

    # Keep the N_MAX most-recent revisions in the date range
    if (nrow(h) > N_MAX_REVISIONS) h <- tail(h, N_MAX_REVISIONS)

    if (nrow(h) < 3) {
      showNotification("Too few revisions in the selected date range.",
                       type = "warning", duration = 6)
      req(FALSE)
    }

    # Re-apply author labels for the current n_top on the filtered, capped
    # subset; label_authors() is O(n) and fast on ≤ N_MAX rows.
    n_top <- max(4L, min(15L, as.integer(input$n_top)))
    h <- h %>%
      mutate(
        author_label = label_authors(user, is_anon, n_top = n_top),
        rev_index    = row_number(),
        size_delta   = size - lag(size, default = first(size))
      )

    min_d <- as.numeric(input$min_delta)
    if (min_d > 0) {
      h <- h %>% filter(row_number() == 1L | abs(size_delta) >= min_d)
    }

    h %>% mutate(
      rev_index  = row_number(),
      size_delta = size - lag(size, default = first(size))
    )
  })

  # ---- Build ownership model (subsampled) -----------------------------------

  own_data <- reactive({
    req(hist_data())
    n_top <- max(4L, min(15L, as.integer(input$n_top)))
    h      <- subsample_history(hist_data(), max_n = 20)
    own_df <- build_content_ownership(h)
    own_c  <- collapse_minor_authors(own_df, n_top = n_top)
    list(h = h, own_df = own_df, own_c = own_c)
  })

  # ---- Author list (left panel) --------------------------------------------

  output$author_list <- renderUI({
    req(hist_data(), own_data())

    h   <- hist_data()
    od  <- own_data()
    cur_disabled <- isolate(user_enabled_rv())
    cur_colors   <- isolate(user_colors_rv())

    OTHER_GRPS <- c("Other", "Anonymous", "Other (registered)")

    # --- Group controls (one row per Sankey group) ---------------------------
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
        toggle_el,
        color_el,
        tags$span(
          style = "flex:1; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
          grp
        ),
        tags$span(style = "color:#888; font-size:10px; white-space:nowrap;", kB_str)
      )
    })

    # --- Editor list (read-only, all 50 editors) ----------------------------
    ed  <- editor_table(h, n_top = 50)
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

  # ---- History flow: echarts4r Sankey diagram --------------------------------

  output$hf_plot <- renderEcharts4r({
    req(hist_data(), nrow(hist_data()) >= 3)

    od            <- own_data()
    disabled      <- user_enabled_rv()
    custom_colors <- user_colors_rv()

    # Merge hidden authors into "Other (hidden)"
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

    # Build effective palette with custom overrides
    all_grps <- unique(own_c$grp)
    pal      <- author_palette(all_grps)
    if ("Other" %in% all_grps) pal[["Other"]] <- "#DDDDDD"
    pal[["Other (hidden)"]] <- "#BBBBBB"
    for (.lbl in names(custom_colors)) {
      if (nzchar(custom_colors[[.lbl]] %||% "")) pal[[.lbl]] <- custom_colors[[.lbl]]
    }

    h_samp   <- od$h
    hl_rev   <- sankey_hl_rev_rv()   # may be NULL

    # rev_index → revid lookup
    rev_revid <- setNames(as.character(h_samp$revid),
                          as.character(h_samp$rev_index))
    max_rev   <- max(own_c$rev_index)

    # -------------------------------------------------------------------
    # Y-axis = position in text (proxy: stack authors by first-appearance)
    # First contributor = top of each column; latest = bottom.
    # Height of each block ∝ bytes owned at that snapshot.
    # -------------------------------------------------------------------
    author_order <- own_c %>%
      group_by(grp) %>%
      summarise(first_rev = min(rev_index), .groups = "drop") %>%
      arrange(first_rev) %>%
      mutate(author_rank = row_number())

    canvas_h <- 420L   # usable pixel height (500px container − title/padding)

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

    # -------------------------------------------------------------------
    # Build Sankey links
    # -------------------------------------------------------------------
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

    # Per-node: y position + colour + highlight border + label on last column only
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

  # Sankey node click → jump to that revision in the selector
  observeEvent(input$hf_click_revid, {
    req(hist_data(), nzchar(input$hf_click_revid))
    h         <- hist_data()
    revid_str <- as.character(round(as.numeric(input$hf_click_revid)))
    if (revid_str %in% as.character(h$revid)) {
      updateSelectInput(session, "rev_selector", selected = revid_str)
    }
  }, ignoreInit = TRUE)

  # ---- Revision selector: populate when hist_data() loads ------------------

  selected_row_rv    <- reactiveVal(NULL)
  wikitext_rv        <- reactiveVal(NULL)
  authorship_rv      <- reactiveVal(NULL)
  diff_rv            <- reactiveVal(NULL)
  sankey_hl_rev_rv   <- reactiveVal(NULL)   # rev_index in own_data()$h to highlight
  user_enabled_rv    <- reactiveVal(character(0))   # labels of HIDDEN authors
  user_colors_rv     <- reactiveVal(list())          # label → custom hex color

  # Reset panel state on new article
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
    # Default to most recent revision
    updateSelectInput(session, "rev_selector",
                      choices  = choices,
                      selected = as.character(h$revid[nrow(h)]))
  }, ignoreInit = FALSE)

  # Toggle author visibility (checkbox in left panel)
  observeEvent(input$toggle_author, {
    req(input$toggle_author)
    lbl <- input$toggle_author$label
    chk <- isTRUE(input$toggle_author$checked)
    cur <- user_enabled_rv()
    user_enabled_rv(if (chk) setdiff(cur, lbl) else union(cur, lbl))
  }, ignoreInit = TRUE)

  # Custom colour for an author group
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

  # Load text + attribution + diff whenever the selected revision changes
  observeEvent(input$rev_selector, {
    req(hist_data(), nzchar(input$rev_selector))

    h <- hist_data()
    # Use character comparison to avoid integer overflow on large revids
    row_idx <- which(as.character(h$revid) == input$rev_selector)
    if (length(row_idx) == 0) {
      showNotification("Revision not found in history.", type = "warning")
      return()
    }
    row_idx <- row_idx[1]
    row     <- h[row_idx, ]

    selected_row_rv(row)
    wikitext_rv(NULL)
    authorship_rv(NULL)
    diff_rv(NULL)

    req(own_data())

    # Highlight the nearest Sankey snapshot for the selected revision
    h_samp   <- own_data()$h
    exact_idx <- which(as.character(h_samp$revid) == input$rev_selector)
    if (length(exact_idx) > 0) {
      sankey_hl_rev_rv(h_samp$rev_index[exact_idx[1]])
    } else {
      # Snap to nearest sampled revision by timestamp
      diffs <- abs(as.numeric(difftime(h_samp$ts_posix, row$ts_posix, units = "secs")))
      sankey_hl_rev_rv(h_samp$rev_index[which.min(diffs)])
    }

    user_label_map <- h %>%
      distinct(user, author_label) %>%
      { setNames(.$author_label, .$user) }
    pal_auth <- author_palette(unique(h$author_label))

    # Attribution: only sample revisions up to the selected one
    h_samp   <- own_data()$h
    h_before <- h_samp[h_samp$ts_posix <= row$ts_posix, , drop = FALSE]
    if (nrow(h_before) == 0) h_before <- h_samp[1, , drop = FALSE]
    idx       <- unique(round(seq(1, nrow(h_before), length.out = min(5L, nrow(h_before)))))
    snap_rows <- h_before[idx, ]

    revid_str <- as.character(row$revid)

    withProgress(message = "Loading revision…", value = 0, {

      incProgress(0.25, detail = "wikitext")
      wt <- fetch_wikitext_for_revid(revid_str)
      wikitext_rv(wt)

      incProgress(0.25, detail = "attributing lines")
      authorship_rv(
        build_line_attribution(wt, snap_rows, pal_auth, user_label_map)
      )

      # Diff against the immediately preceding revision in hist_data()
      if (row_idx > 1) {
        incProgress(0.25, detail = "fetching prev. revision")
        prev_revid <- as.character(h$revid[row_idx - 1])
        wt_prev    <- fetch_wikitext_for_revid(prev_revid)
        incProgress(0.25, detail = "computing diff")
        diff_rv(compute_line_diff(wt_prev, wt))
      } else {
        diff_rv(list(first_rev = TRUE))
      }
    })
  }, ignoreInit = TRUE)

  # ---- Revision metadata ---------------------------------------------------

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

  # ---- Wikitext panel (coloured by author) ---------------------------------

  output$wikitext_panel <- renderUI({
    auth <- authorship_rv()
    wt   <- wikitext_rv()

    if (!is.null(auth)) {
      tbl_rows <- if (!is.null(auth$summary_df) && nrow(auth$summary_df) > 0) {
        sd <- auth$summary_df
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
      } else NULL

      tagList(
        tags$div(style = "font-size:10px; color:#777; margin-bottom:4px;",
                 sprintf("Line attribution · %d revision samples · hover lines for details",
                         auth$n_snaps)),
        div(class = "wikibox", HTML(auth$html)),
        tbl_rows
      )
    } else if (!is.null(wt)) {
      wt_esc <- htmlEscape(wt)
      wt_hl  <- gsub("(&lt;ref[^/].*?&lt;/ref&gt;|&lt;ref\\s[^/]*/?&gt;)",
                     "<mark>\\1</mark>", wt_esc, perl = TRUE)
      div(class = "wikibox", HTML(wt_hl))
    } else {
      tags$span(style = "color:#999; font-family:inherit; font-size:11px;",
                "Select a revision from the dropdown above.")
    }
  })

  # ---- Diff panel ----------------------------------------------------------

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

    lines_html <- character(0)

    # Removed lines (red)
    removed_show <- head(d$removed, 60)
    for (l in removed_show) {
      lines_html <- c(lines_html, sprintf(
        '<span class="diff-removed">− %s</span>',
        htmlEscape(l)
      ))
    }
    if (d$n_removed > 60) {
      lines_html <- c(lines_html, sprintf(
        '<span class="diff-note">… %d more removed lines not shown</span>',
        d$n_removed - 60
      ))
    }

    # Separator when both additions and deletions
    if (d$n_removed > 0 && d$n_added > 0) {
      lines_html <- c(lines_html, '<span class="diff-note" style="border-top:1px solid #ddd;display:block;margin:4px 0;"></span>')
    }

    # Added lines (green)
    added_show <- head(d$added, 60)
    for (l in added_show) {
      lines_html <- c(lines_html, sprintf(
        '<span class="diff-added">+ %s</span>',
        htmlEscape(l)
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

  # ---- Citation analysis tab -----------------------------------------------

  cite_data_rv <- reactiveVal(NULL)

  observeEvent(input$analyse_citations, {
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
        Sys.sleep(0.15)
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
  })

  output$cite_plot <- renderPlotly({
    cd  <- cite_data_rv()
    req(cd, nrow(cd$mat) > 0)

    type_pal <- c(
      "Journal"       = "#377EB8",
      "Book"          = "#4DAF4A",
      "Web"           = "#FF7F00",
      "News/Magazine" = "#984EA3",
      "Preprint"      = "#E41A1C",
      "Thesis"        = "#F781BF",
      "Conference"    = "#A65628",
      "Report"        = "#F4A460",
      "Multimedia"    = "#FF69B4",
      "Legal/Patent"  = "#808080",
      "Social Media"  = "#1DA1F2",
      "DOI"           = "#00BCD4",
      "Other"         = "#AAAAAA"
    )

    mat     <- cd$mat
    display <- input$cite_display
    type_f  <- input$cite_type_filter

    if (display == "Active in latest revision") mat <- filter(mat, still_present)
    if (display == "Additions only (not in v1)") mat <- filter(mat, first_rev > min(mat$first_rev))
    if (display == "Removed only")              mat <- filter(mat, !still_present)
    if (type_f != "All types") mat <- filter(mat, type == type_f)

    req(nrow(mat) > 0)

    mat <- mat %>%
      mutate(
        status  = if_else(still_present, "Active", "Removed"),
        y_order = row_number()
      )

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

  # ---- Wikipedia article hyperlink above tabs --------------------------------

  output$wiki_link_ui <- renderUI({
    req(raw_hist())
    art <- unique(raw_hist()$art)[1]
    if (is.null(art) || !nzchar(art)) return(NULL)
    wiki_url <- paste0("https://en.wikipedia.org/wiki/",
                       utils::URLencode(gsub(" ", "_", art), reserved = FALSE))
    div(class = "wiki-art-link",
        tags$a(href = wiki_url, target = "_blank", rel = "noopener noreferrer",
               paste0(art, " ↗ Wikipedia")))
  })

  # ---- Citation table: clickable links + XLSX download ----------------------

  if (has_DT) {
    output$cite_table <- DT::renderDT({
      cd   <- cite_data_rv()
      epmc <- epmc_rv()
      req(cd)
      df <- cd$mat %>%
        mutate(
          .url      = vapply(cite_raw, function(x) cite_url(x) %||% "", character(1L)),
          .doi      = vapply(cite_raw, function(x) {
            m <- regmatches(x, regexpr("10\\.\\d{4,}/[-._;()/:a-zA-Z0-9]+", x, perl = TRUE))
            if (length(m) && nzchar(m)) m[[1]] else NA_character_
          }, character(1L)),
          Citation  = mapply(make_cite_link, label, .url),
          Type      = type,
          Added     = format(first_ts, "%Y-%m-%d"),
          `Last seen` = format(last_ts, "%Y-%m-%d"),
          Status    = if_else(still_present, "Active ✓", "Removed"),
          Snapshots = n_snapshots
        )
      if (!is.null(epmc) && nrow(epmc) > 0 && ".doi" %in% names(df)) {
        idx <- match(df$.doi, epmc$doi)
        df$Journal   <- epmc$journalTitle[idx]
        df$Year      <- epmc$pubYear[idx]
        df$OA        <- ifelse(!is.na(idx) & epmc$isOpenAccess[idx] == "Y", "OA ✓", "")
        df$Citations <- epmc$citedByCount[idx]
        df <- df %>% select(Citation, Type, Journal, Year, OA, Citations,
                            Added, `Last seen`, Status, Snapshots)
      } else {
        df <- df %>% select(Citation, Type, Added, `Last seen`, Status, Snapshots)
      }
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
      cd <- cite_data_rv()
      req(cd)
      cd$mat %>%
        mutate(
          Added       = format(first_ts, "%Y-%m-%d"),
          `Last seen` = format(last_ts,  "%Y-%m-%d"),
          Status      = if_else(still_present, "Active", "Removed")
        ) %>%
        select(label, type, Added, `Last seen`, Status, n_snapshots) %>%
        rename(Citation = label, Type = type, Snapshots = n_snapshots)
    })
  }

  # ---- XLSX download: citation table ----------------------------------------

  if (has_xlsx) {
    output$dl_cite_xlsx <- downloadHandler(
      filename = function() {
        art <- gsub("[^A-Za-z0-9_-]", "_", trimws(input$article))
        paste0(art, "_citations.xlsx")
      },
      content = function(file) {
        cd <- cite_data_rv()
        req(cd)
        df <- cd$mat %>%
          mutate(
            URL       = vapply(cite_raw, function(x) cite_url(x) %||% "", character(1L)),
            Added     = format(first_ts, "%Y-%m-%d"),
            `Last seen` = format(last_ts, "%Y-%m-%d"),
            Status    = if_else(still_present, "Active", "Removed")
          ) %>%
          select(label, type, Added, `Last seen`, Status, n_snapshots, URL, cite_raw) %>%
          rename(Citation = label, Type = type, Snapshots = n_snapshots, `Raw wikitext` = cite_raw)
        openxlsx::write.xlsx(df, file)
      }
    )
  }

  # ── Authorship tab ─────────────────────────────────────────────────────────

  auth_stats_rv <- reactive({
    req(hist_data())
    authorship_stats(hist_data())
  })

  auth_editors_rv <- reactive({
    req(hist_data())
    editor_table(hist_data(), n_top = 100L)
  })

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

  output$auth_anon_plot <- renderPlot({
    req(hist_data())
    print(plot_anon_vs_registered(hist_data()))
  }, res = 96)

  output$auth_top_plot <- renderPlot({
    req(hist_data())
    print(plot_top_editors(hist_data()))
  }, res = 96)

  output$auth_cum_plot <- renderPlotly({
    req(hist_data())
    plot_cumulative_edits(hist_data())
  })

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

  # ── Stability tab ──────────────────────────────────────────────────────────

  stab_stats_rv <- reactive({
    req(hist_data())
    stability_stats(hist_data())
  })

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

  output$stab_size_plot <- renderPlotly({
    req(hist_data())
    plot_size_timeline(hist_data(), interactive = TRUE)
  })

  output$stab_delta_plot <- renderPlot({
    req(hist_data())
    print(plot_delta_distribution(hist_data()))
  }, res = 96)

  output$stab_rhythm_plot <- renderPlot({
    req(hist_data())
    print(plot_editing_rhythm(hist_data()))
  }, res = 96)

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

  # ── Vandalism & Edit Wars tab ──────────────────────────────────────────────

  vandalism_rv <- reactive({
    req(hist_data())
    h <- detect_vandalism(hist_data())
    s <- compute_survival(h)
    list(hist = h, survival = s)
  })

  edit_war_rv <- reactive({
    req(hist_data())
    detect_edit_wars(hist_data())
  })

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

  # ── SciScore gauge cards ────────────────────────────────────────────────────

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

  # ── EuropePMC annotation ────────────────────────────────────────────────────

  epmc_rv <- reactiveVal(NULL)

  observeEvent(input$annotate_epmc, {
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
    withProgress(message = sprintf("Querying EuropePMC for %d DOIs…", length(dois)), {
      res <- tryCatch(
        wikilite::annotate_doi_list_europmc(dois),
        error = function(e) {
          showNotification(paste("EuropePMC error:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })
    epmc_rv(res)
    if (!is.null(res))
      showNotification(sprintf("Annotated %d / %d DOIs.", nrow(res), length(dois)),
                       type = "message")
  })

  # ── Citation latency ────────────────────────────────────────────────────────

  latency_rv <- reactiveVal(NULL)

  observeEvent(input$compute_latency, {
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
      withProgress(message = "Querying EuropePMC for latency…", {
        epmc_ann <- tryCatch(
          wikilite::annotate_doi_list_europmc(unique(doi_rows$citation_fetched)),
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
  })

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

  # ── BibTeX export ───────────────────────────────────────────────────────────

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

  # ── Multi-Article tab ───────────────────────────────────────────────────────

  multi_data_rv <- reactiveVal(NULL)

  observeEvent(input$multi_fetch, {
    arts_raw <- trimws(input$multi_articles)
    req(nzchar(arts_raw))
    arts <- unique(trimws(strsplit(arts_raw, "\n")[[1]]))
    arts <- arts[nzchar(arts)]
    if (length(arts) < 2) {
      showNotification("Enter at least 2 article titles.", type = "warning")
      return()
    }
    multi_data_rv(list(articles = arts,
                       lang     = input$multi_lang %||% "en",
                       color_by = input$multi_color_by %||% "sciscore"))
    showNotification(sprintf("Loaded %d articles — select a sub-tab to render.",
                             length(arts)), type = "message")
  })

  output$multi_timeline <- renderPlotly({
    d <- multi_data_rv()
    req(!is.null(d))
    withProgress(message = "Building timeline…", {
      tryCatch(
        wikilite::plot_interactive_timeline(d$articles, lang = d$lang,
                                            color_by = d$color_by),
        error = function(e) {
          showNotification(paste("Timeline error:", conditionMessage(e)), type = "error")
          plotly::plotly_empty()
        }
      )
    })
  })

  if (has_vis) {
    output$multi_cocite <- renderVisNetwork({
      d <- multi_data_rv()
      req(!is.null(d))
      withProgress(message = "Building co-citation network…", {
        tryCatch(
          wikilite::plot_article_cocitation_network(d$articles, lang = d$lang),
          error = function(e) {
            showNotification(paste("Co-citation error:", conditionMessage(e)), type = "error")
            NULL
          }
        )
      })
    })

    output$multi_pubnet <- renderVisNetwork({
      d <- multi_data_rv()
      req(!is.null(d))
      withProgress(message = "Building publication network…", {
        tryCatch(
          wikilite::plot_article_publication_network(d$articles, lang = d$lang),
          error = function(e) {
            showNotification(paste("Publication network error:", conditionMessage(e)), type = "error")
            NULL
          }
        )
      })
    })

    output$multi_wikilink <- renderVisNetwork({
      d <- multi_data_rv()
      req(!is.null(d))
      withProgress(message = "Building wikilink network…", {
        tryCatch(
          wikilite::plot_article_wikilink_network(d$articles, lang = d$lang),
          error = function(e) {
            showNotification(paste("Wikilink error:", conditionMessage(e)), type = "error")
            NULL
          }
        )
      })
    })
  }

  # ── Category Explorer ───────────────────────────────────────────────────────

  cat_data_rv <- reactiveVal(NULL)

  observeEvent(input$cat_fetch, {
    cat_name <- trimws(input$cat_name)
    req(nzchar(cat_name))
    lang <- input$cat_lang %||% "en"
    withProgress(message = paste0("Loading category: ", cat_name, " …"), {
      pages <- tryCatch(
        wikilite::get_pagename_in_cat(cat_name, lang = lang),
        error = function(e) {
          showNotification(paste("Category error:", conditionMessage(e)), type = "error")
          character(0)
        }
      )
      subcats <- tryCatch(
        wikilite::get_subcat_table(cat_name, lang = lang),
        error = function(e) NULL
      )
      cat_data_rv(list(pages = pages, subcats = subcats,
                       cat = cat_name, lang = lang))
    })
    showNotification(
      sprintf("Category '%s': %d articles.", cat_name,
              length(cat_data_rv()$pages %||% character(0))),
      type = "message"
    )
  })

  output$cat_info <- renderUI({
    d <- cat_data_rv()
    req(!is.null(d))
    n_art <- length(d$pages %||% character(0))
    n_sub <- if (!is.null(d$subcats) && is.data.frame(d$subcats)) nrow(d$subcats) else 0L
    div(class = "metric-row",
        tags$div(class = "metric-card",
                 tags$div(class = "metric-val", format(n_art, big.mark = ",")),
                 tags$div(class = "metric-lbl", "Articles")),
        tags$div(class = "metric-card",
                 tags$div(class = "metric-val", format(n_sub, big.mark = ",")),
                 tags$div(class = "metric-lbl", "Subcategories"))
    )
  })

  output$cat_articles_ui <- renderUI({
    d <- cat_data_rv()
    req(!is.null(d), length(d$pages) > 0)
    lang <- d$lang %||% "en"
    tags$ul(
      style = "max-height:300px; overflow-y:auto; padding-left:16px;",
      lapply(d$pages, function(p) {
        url <- paste0("https://", lang, ".wikipedia.org/wiki/",
                      utils::URLencode(gsub(" ", "_", p), reserved = FALSE))
        tags$li(tags$a(href = url, target = "_blank", rel = "noopener noreferrer", p))
      })
    )
  })

  if (has_DT) {
    output$cat_subcat_tbl <- DT::renderDT({
      d <- cat_data_rv()
      req(!is.null(d), !is.null(d$subcats))
      d$subcats
    }, options = list(pageLength = 10, dom = "ftp"),
    rownames = FALSE, class = "compact stripe hover")
  } else {
    output$cat_subcat_base <- renderTable({
      d <- cat_data_rv()
      req(!is.null(d))
      d$subcats
    })
  }

  observeEvent(input$cat_send_multi, {
    d <- cat_data_rv()
    req(!is.null(d), length(d$pages) > 0)
    updateTextAreaInput(session, "multi_articles",
                        value = paste(d$pages, collapse = "\n"))
    updateTabsetPanel(session, "main_tabs", selected = "Multi-Article")
    showNotification(
      sprintf("Sent %d articles to Multi-Article tab.", length(d$pages)),
      type = "message"
    )
  })
}

# ---------------------------------------------------------------------------
shinyApp(ui, server)
