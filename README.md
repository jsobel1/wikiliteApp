# wikiliteApp

An R package shipping a multi-tab interactive **Shiny application** for
exploring Wikipedia article revision history, citation structure, authorship
patterns, content stability, and vandalism — built on the
[**wikilite**](https://github.com/jsobel1/wikilite) data backend.

The visualisation approach follows
[Viégas, Wattenberg & Dave (CHI 2004)](https://doi.org/10.1145/985692.985765),
the original Microsoft Wikipedia History Flow study.

---

## Installation

```r
# Install the wikilite data backend first:
remotes::install_github("jsobel1/wikilite")

# Then install this app package:
remotes::install_github("jsobel1/wikiliteApp")
```

**Optional extras** (unlock additional features):

```r
install.packages(c(
  "DT",          # interactive sortable / filterable tables
  "openxlsx",    # every XLSX export
  "visNetwork",  # all corpus network plots
  "httr",        # recommended; needed for the batched / cached fetchers
  "rcrossref",   # BibTeX export
  "europepmc",   # fallback DOI annotation
  "ggalluvial",  # alluvial history flow
  "cowplot"      # side-by-side comparisons
))
```

---

## Quick start

```r
library(wikiliteApp)
run_app()        # opens in your default browser
```

---

## App structure

The top navbar exposes three modes:

* **Single Article** — full multi-tab analysis of one article. Sidebar on the
  left holds article search, date range, sampling controls and a Fetch button;
  results appear in tabs on the right.
* **Corpus Analysis** — define one or more named, optionally category-linked
  article lists and compare them across timeline, citations and three
  networks. The *Render comparison →* button at the bottom of the corpus
  sidebar commits the current corpora to the right-hand panels.
* **Help** — full in-app user guide (parameter reference, tab descriptions,
  data sources, citation).

### Single Article tabs

| Tab | What it shows |
|-----|---------------|
| **History Flow** | Sankey-style ribbon coloured by editor; revert events highlighted. Click a node to jump to that revision in the Revision Inspector. |
| **Citations** | Timeline + type-counts bar chart of `<ref>` citations, status / type filters, SciScore cards, batched DOI annotation (EuropePMC), ISBN annotation (Google Books), citation latency, BibTeX & XLSX exports. |
| **Authorship** | Anonymous vs. registered share, top editors, cumulative edits, Gini, full editor table with revert rates. |
| **Stability** | Article size timeline, Δ-byte distribution, editing-rhythm heatmap, plus a **SciScore over time** sub-section (per-snapshot journal % and DOI %, citation type mix). |
| **Vandalism & Wars** | Heuristic vandalism detection, repair-time survival, edit-war episodes. |
| **Revision Inspector** | Token-level authorship via the [WikiWho API](https://wikiwho-api.wmcloud.org/) with line-diff fallback; line-level diff vs. previous revision. |

### Corpus Analysis tabs

| Tab | What it shows |
|-----|---------------|
| **Timeline** | One marker per article positioned at its creation date, coloured by corpus, sized by total revisions. Click to open Wikipedia. Y-axis grouping selector (corpus / per-article). |
| **Citations** | Cross-corpus citation extraction with per-corpus *and* per-article views: reference composition (counts or %), SciScore per corpus + per article (box plot or sorted bar chart), batched EuropePMC and Google Books annotation, multi-sheet XLSX exports. |
| **Co-citation Network** | Articles sharing one or more DOIs, coloured by corpus, edge weight = #shared. Click a node → Wikipedia. |
| **Publication Network** | Bipartite Article ↔ Publication graph with `Min wiki` / `Top N DOIs` thresholds. Click a publication → `doi.org`. |
| **Wikilink Network** | Outgoing `[[wikilink]]` edges from corpus articles; threshold + corpus-only toggle; click any node → Wikipedia. |

---

## Performance & robustness

* All MediaWiki / WikiWho / xtools / EuropePMC / Google Books calls go through
  `httr` with a descriptive User-Agent and exponential backoff on 429 / 5xx
  responses. The base R `url()` path is throttled aggressively and avoided.
* Per-session caches for raw history (on disk via `tools::R_user_dir()`),
  article info, latest wikitext, DOI annotations and ISBN annotations.
  Re-rendering or re-annotating after a parameter change is near-instant.
* EuropePMC annotations use batched `OR`-queries (~25 DOIs per request,
  ~25× faster than the per-DOI fallback).
* Sampling (`Most recent` / `Even` / `Random`) happens **before** the heavy
  `prepare_history()` step, so 100 k+ revision articles stay responsive.
* Heavy observers are wrapped: transient API failures surface as toast
  notifications instead of disconnecting the Shiny session.
* Numeric / date inputs are debounced (300–600 ms) so rapid spinner clicks
  don't trigger a cascade of rebuilds.

---

## Standalone R functions

All analysis modules are also exported as regular R functions for scripted
use:

```r
library(wikiliteApp)

# Fetch and prepare revision history
hist <- fetch_history("Zeitgeber", lang = "en")
hist <- prepare_history(hist)

# Authorship
authorship_stats(hist)
editor_table(hist)
plot_top_editors(hist)

# Stability
stability_stats(hist)
plot_size_timeline(hist)

# Vandalism
hist <- detect_vandalism(hist)
surv <- compute_survival(hist)
summarise_survival(surv)

# Edit wars
wars <- detect_edit_wars(hist)
plot_edit_wars(hist, wars)

# History flow visualisations
plot_history_flow(hist, spacing = "date")
plot_history_flow_alluvial(hist, max_revisions = 30)
plot_history_flow_sankey(hist)

# Helpers shared with the Shiny app
sample_revisions(hist, n = 1500, method = "even")
extract_cites(wikitext)
cite_type(citation_string)
extract_isbn(citation_string)
compute_line_diff(old_wikitext, new_wikitext)
```

---

## Tests

The shared helpers have a `testthat` suite:

```r
devtools::test()   # 85 tests covering sampling, citation classification,
                   # ISBN extraction, line-diff and hex-to-rgba helpers
```

---

## Data sources

| Source | Used for |
|--------|----------|
| [MediaWiki Action API](https://www.mediawiki.org/wiki/API:Main_page) | Revision history, wikitext, opensearch, category members |
| [WikiWho](https://wikiwho-api.wmcloud.org/) | Token-level authorship in the Revision Inspector |
| [xtools](https://xtools.wmcloud.org/) | Article metadata for the corpus timeline |
| [EuropePMC REST API](https://europepmc.org/RestfulWebService) | DOI metadata + citation latency |
| [Google Books API](https://developers.google.com/books) | ISBN metadata |

For a programmatic interface to the same functionality (no GUI), use
[**wikilite**](https://github.com/jsobel1/wikilite) directly or the
[**wikicitation-mcp**](https://github.com/jsobel1/wikicitation-mcp) MCP server
for Claude integration.

---

## Citation

If you use this tool in your research, please cite:

> Viégas, F. B., Wattenberg, M., & Dave, K. (2004). Studying cooperation and
> conflict between authors with history flow visualizations. *Proceedings of
> CHI 2004*, 575–582. <https://doi.org/10.1145/985692.985765>

---

## License

MIT © Jonathan Sobel
