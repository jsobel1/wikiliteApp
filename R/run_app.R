#' Launch the Wikipedia History Flow Shiny app
#'
#' Opens the interactive app in your default browser (or the RStudio viewer pane
#' when called from RStudio).  The app lets you explore a Wikipedia article's
#' revision history, citation structure, authorship patterns, stability, and
#' vandalism using the **wikilite** package as the data backend.
#'
#' @param ... Additional arguments passed to [shiny::runApp()], e.g.
#'   `port = 4321` or `launch.browser = FALSE`.
#'
#' @return Invisible `NULL`.  Called for its side effect of launching the app.
#'
#' @seealso [wikilite](https://github.com/jsobel1/wikilite) — the data backend.
#'
#' @examples
#' \dontrun{
#'   wikiliteApp::run_app()
#' }
#'
#' @export
run_app <- function(...) {
  app_dir <- system.file("shiny", package = "wikiliteApp")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. ",
         "Try re-installing wikiliteApp.")
  }
  shiny::runApp(app_dir, ...)
}
