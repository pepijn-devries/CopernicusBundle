marineAssetUI <- function(id) {
  ns <- NS(id)
  tagList(
    "TODO"
  )
}

marineAssetServer <- function(id, asset) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        
        browser() #TODO
        if (is.null(asset())) return(NULL)
        vsi <- CopernicusMarine:::.uri_to_vsi(asset()$href)
#TODO
        #        proxy <- CopernicusMarine:::.get_stars_proxy(vsi, NULL)
        asset()
      })
      
      return(reactive({ }))
    }
  )
}