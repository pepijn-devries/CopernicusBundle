marineAssetUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    "TODO"
  )
}

marineAssetServer <- function(id, asset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::observe({
        
        if (is.null(asset())) return(NULL)
        vsi <- CopernicusMarine:::.uri_to_vsi(asset()$href)
#TODO
        #        proxy <- CopernicusMarine:::.get_stars_proxy(vsi, NULL)
        asset()
      })
      
      return(shiny::reactive({ }))
    }
  )
}