#' @export
cb_dashboard <- function(...) {
  app_src <- system.file("dashboard", package = "CopernicusBundle")
  shiny::runApp(app_src)
}