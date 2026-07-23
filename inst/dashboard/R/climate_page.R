climatePageUI <- function(id) {
  ns <- shiny::NS(id)
  if (requireNamespace("CopernicusClimate")) {
    bslib::page_navbar(
      id = ns("climate_nav"),
      bslib::nav_panel("Search",       climateSearchUI(ns("searchMod"))),
      bslib::nav_panel("Prepare Job",  climatePrepareUI(ns("prepareMod")),
                       value = "climate_prepare"),
      bslib::nav_panel("Request Job",  climateRequestUI(ns("requestMod")),
                       value = "climate_request"),
      bslib::nav_panel("Jobs",         climateJobsUI(ns("jobsMod")),
                       value = "climate_jobs"),
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
  shiny::moduleServer(
    id,
    function(input, output, session) {
      if (!requireNamespace("CopernicusClimate"))
        return(shiny::reactive({ }))
      search  <- climateSearchServer("searchMod")
      prepare <- climatePrepareServer("prepareMod", search)
      request <- climateRequestServer("requestMod", prepare)
      jobs    <- climateJobsServer("jobsMod")
      licens  <- climateLicensesServer("licensesMod")
      account <- climateAccountServer("accountMod")
      
      shiny::observeEvent( search(), {
        bslib::nav_select("climate_nav", "climate_prepare")
      })
      
      shiny::observeEvent( prepare(), {
        bslib::nav_select("climate_nav", "climate_request")
      })
      
      shiny::observeEvent( request(), {
        bslib::nav_select("climate_nav", "climate_jobs")
      })
      
      shiny::observe({ jobs() })
      shiny::observe({ licens() })
      shiny::observe({ account() })
      
      return(shiny::reactive({ }))
    }
  )
}