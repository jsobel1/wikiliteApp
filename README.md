# wikiliteApp

An R package that ships a full-featured interactive **Shiny application** for
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
  "DT",          # interactive citation table
  "openxlsx",    # Excel export
  "visNetwork",  # network visualisations
  "rcrossref",   # BibTeX export
  "europepmc",   # DOI annotation
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

## App tabs

| Tab | What it shows |
|-----|---------------|
| **History Flow** | Stacked area chart coloured by editor; revert events overlaid as red triangles |
| **Citation Analysis** | Citation timeline, SciScore gauge, DOI annotation, BibTeX export, latency panel |
| **Authorship** | Anonymous vs. registered breakdown, editor Gini coefficient, top-editor charts |
| **Stability** | Article size over time, edit-size distribution, editing rhythm heatmap |
| **Vandalism & Wars** | Mass-deletion detection, repair-time survival curve, edit-war episodes |
| **Multi-Article** | Compare articles with interactive Gantt timeline, co-citation / publication / wikilink networks |
| **Category Explorer** | Browse the Wikipedia category tree; send articles to Multi-Article in one click |

---

## Standalone R functions

All analysis modules are also exported as regular R functions for scripted use:

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
```

---

## Data backend

All Wikipedia data is retrieved via the
[**wikilite**](https://github.com/jsobel1/wikilite) package, which wraps the
MediaWiki Action API and provides citation parsing, SciScore computation, DOI
annotation, and category browsing.

For a lightweight, programmatic interface to the same functionality (no GUI),
use `wikilite` directly or the
[**wikicitation-mcp**](https://github.com/jsobel1/wikicitation-mcp) MCP server
for Claude integration.

---

## Citation

If you use this tool in your research, please cite:

> Viégas, F.B., Wattenberg, M., & Dave, K. (2004). Studying cooperation and
> conflict between authors with history flow visualizations. *Proceedings of
> CHI 2004*, 575–582. <https://doi.org/10.1145/985692.985765>

---

## License

MIT © Jonathan Sobel
