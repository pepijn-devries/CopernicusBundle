dataspacePageUI <- function(id) {
  ns <- NS(id)
  if (requireNamespace("CopernicusDataspace")) {
    bslib::page_navbar(
      bslib::nav_panel("Collections", dataspaceCollectionsUI(ns("collectionsMod"))),
      bslib::nav_panel("Search",      dataspaceSearchUI(ns("searchMod"))),
      bslib::nav_panel("Account",     dataspaceAccountUI(ns("accountMod")))
    )
  } else {
    bslib::page(
      "Install package CopernicusMarine first if needed"
    )
  }
}

dataspacePageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      if (!requireNamespace("CopernicusDataspace"))
        return(reactive({ }))
      collections <- dataspaceCollectionsServer("collectionsMod")
      search      <- dataspaceSearchServer("searchMod", collections)
      account     <- dataspaceAccountServer("accountMod")
      
      observe({ collections() })
      observe({ search() })
      observe({ account() })
      
      return(reactive({ }))
    }
  )
}