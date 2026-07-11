marinePageUI <- function(id) {
  ns <- NS(id)
  if (requireNamespace("CopernicusMarine")) {
    bslib::page_navbar(
      bslib::nav_panel("Search",       marineSearchUI(ns("searchMod"))),
      bslib::nav_panel("Account",      marineAccountUI(ns("accountMod")))
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
      account <- marineAccountServer("accountMod")
      
      observe({ search() })
      observe({ account() })
      
      return(reactive({ }))
    }
  )
}