marineAssetUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    zarrUI(ns("zarr_mod"))
  )
}

marineAssetServer <- function(id, asset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      zarr <- zarrServer("zarr_mod", asset)

      shiny::observe({ zarr() })
      
      return(shiny::reactive({ }))
    }
  )
}