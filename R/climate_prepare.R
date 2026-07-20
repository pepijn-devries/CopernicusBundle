#' TODO
#' 
#' TODO
#' @param product_id TODO
#' @param ... TODO
#' @examples
#' if (interactive()) {
#'   cb_climate_prepare_job("reanalysis-era5-land")
#' }
#' @export
cb_climate_prepare_job <- function(product_id, ...) {
  if (!requireNamespace("CopernicusClimate")) {
    stop("TODO")
  }
  app_src <- system.file("climate_prepare", package = "CopernicusBundle")
  if (missing(product_id)) product_id <- NULL
  shiny::shinyOptions(product_id = product_id)
  shiny::runGadget(shiny::shinyAppDir(app_src), ...)
}