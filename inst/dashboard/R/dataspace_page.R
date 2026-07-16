dataspacePageUI <- function(id) {
  ns <- NS(id)
  if (requireNamespace("CopernicusDataspace")) {
    bslib::page_navbar(
      id = ns("dataspace_nav"),
      bslib::nav_panel("Collections",    dataspaceCollectionsUI(ns("collectionsMod"))),
      bslib::nav_panel("Search",         dataspaceSearchUI(ns("searchMod")),
                       value = "dataspace_search"),
      bslib::nav_panel("Search Results", dataspaceSearchResultUI(ns("searchResMod")),
                       value = "dataspace_results"),
      bslib::nav_panel("Account",        dataspaceAccountUI(ns("accountMod")))
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
      searchRes   <- dataspaceSearchResultServer("searchResMod", search)
      account     <- dataspaceAccountServer("accountMod")
      
      observeEvent(
        collections(), {
          bslib::nav_select("dataspace_nav", "dataspace_search")
        })
      
      observeEvent(
        search(), {
          bslib::nav_select("dataspace_nav", "dataspace_results")
        })
      
      observe({ searchRes() })
      observe({ account() })
      
      return(reactive({ }))
    }
  )
}