marinePageUI <- function(id) {
  ns <- shiny::NS(id)
  if (requireNamespace("CopernicusMarine")) {
    bslib::page_navbar(
      id = ns("marine_nav"),
      bslib::nav_panel("Search",  marineSearchUI(ns("searchMod"))),
      bslib::nav_panel("Product", marineProductUI(ns("productMod")),
                       value = "marine_product"),
      bslib::nav_panel("Asset",   marineAssetUI(ns("assetMod")),
                       value = "marine_asset"),
      bslib::nav_panel("Account", marineAccountUI(ns("accountMod")))
    )
  } else {
    bslib::page(
      "Install package CopernicusMarine first if needed"
    )
  }
}

marinePageServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      if (!requireNamespace("CopernicusMarine"))
        return(shiny::reactive({ }))
      search  <- marineSearchServer("searchMod")
      product <- marineProductServer("productMod", search)
      asset   <- marineAssetServer("assetMod", product)
      account <- marineAccountServer("accountMod")

      shiny::observeEvent( search(), {
        bslib::nav_select("marine_nav", "marine_product")
      })

      shiny::observeEvent( product(), {
        bslib::nav_select("marine_nav", "marine_asset")
      })
      
      shiny::observe({ product() })
      shiny::observe({ account() })
      
      return(shiny::reactive({ }))
    }
  )
}