marineAssetUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_hidden(
    id = ns("asset_switcher"),
    selected = "empty_panel",
    bslib::nav_panel_hidden(
      value = "zarr_panel",
      zarrUI(ns("zarr_mod"))
    ),
    bslib::nav_panel_hidden(
      value = "native_panel",
      nativeUI(ns("native_mod"))
    ),
    bslib::nav_panel_hidden(
      value = "wmts_panel",
      wmtsUI(ns("wmts_mod"))
    ),
    bslib::nav_panel_hidden(
      value = "empty_panel",
      "Select an asset first"
    )
  )
}

marineAssetServer <- function(id, asset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      zarr   <- zarrServer("zarr_mod", asset)
      native <- nativeServer("native_mod", asset)
      wmts   <- nativeServer("wmts_mod", asset)
      
      observeEvent(asset(), {
        ast <- asset()
        if (is.null(ast) || nrow(ast$layer) == 0) {
          ast <- "empty"
        } else {
          is_zarr <-
            ast$layer$assets[[1]][[ast$asset]]$href |>
            tolower() |>
            endsWith(".zarr")
          ast <- ifelse(is_zarr, "zarr", ast$asset)
          if (!ast %in% c("zarr", "native", "wmts"))
            ast <- "empty"
        }
        bslib::nav_select("asset_switcher", paste0(ast, "_panel"))
      })

      shiny::observe({ zarr() })
      
      return(shiny::reactive({ }))
    }
  )
}