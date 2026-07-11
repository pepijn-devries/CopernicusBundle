marinePageUI <- function(id) {
  ns <- NS(id)
  if (requireNamespace("CopernicusMarine")) {
    bslib::page_navbar(
      bslib::nav_panel("Search",  marineSearchUI(ns("searchMod"))),
      bslib::nav_panel("Product", marineProductUI(ns("productMod"))),
      bslib::nav_panel("Account", marineAccountUI(ns("accountMod")))
    )
  } else {
    bslib::page(
      "Install package CopernicusMarine first if needed"
    )
  }
}

marinePageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      if (!requireNamespace("CopernicusMarine"))
        return(reactive({ }))
      search  <- marineSearchServer("searchMod")
      product <- marineProductServer("productMod", search)
      account <- marineAccountServer("accountMod")
      
      observe({ search() })
      observe({ product() })
      observe({ account() })
      
      return(reactive({ }))
    }
  )
}