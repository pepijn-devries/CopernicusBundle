climatePageUI <- function(id) {
  ns <- NS(id)
  if (requireNamespace("CopernicusClimate")) {
    bslib::page_navbar(
      bslib::nav_panel("Search",       climateSearchUI(ns("searchMod"))),
      bslib::nav_panel("Prepare Jobs", climatePrepareUI(ns("prepareMod"))),
      bslib::nav_panel("Jobs",         climateJobsUI(ns("jobsMod"))),
      bslib::nav_panel("Licenses",     climateLicensesUI(ns("licensesMod"))),
      bslib::nav_panel("Account",      climateAccountUI(ns("accountMod")))
    )
  } else {
    bslib::page(
      "Install package CopernicusClimate first if needed"
    )
  }
}

climatePageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      if (!requireNamespace("CopernicusClimate"))
        return(reactive({ }))
      search  <- climateSearchServer("searchMod")
      prepare <- climatePrepareServer("prepareMod", search)
      jobs    <- climateJobsServer("jobsMod")
      licens  <- climateLicensesServer("licensesMod")
      account <- climateAccountServer("accountMod")
      
      observe({ search() })
      observe({ prepare() })
      observe({ jobs() })
      observe({ licens() })
      observe({ account() })
      
      return(reactive({ }))
    }
  )
}