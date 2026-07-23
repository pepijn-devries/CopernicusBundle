zarrUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    "TODO dit wordt zarr",
    shiny::uiOutput(ns("dimensionUI"))
  )
}

zarrServer <- function(id, asset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      get_proxy <- shiny::reactive({
        ast <- asset()
        if (!is.null(unlist(ast))) {
          href <- ast$layer$assets[[1]][[ast$asset]]$href
          if (endsWith(toupper(href), ".ZARR")) {
            tryCatch({
              CopernicusMarine::cms_zarr_proxy(
                ast$layer$collection,
                ast$layer$id,
                ast$variable,
                ast$asset)
            }, error = \(e) NULL)
          } else {
            NULL
          }
        } else {
          NULL
        }
      })
      
      output$dimensionUI <- shiny::renderUI({
        proxy <- get_proxy()
        if (!is.null(proxy)) {
          proxy |> capture.output() |> paste(collapse = "\n")
        }
      })
      
      return(shiny::reactive({ }))
    }
  )
}