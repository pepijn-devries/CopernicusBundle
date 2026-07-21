future::plan(future::multisession) ## In order to handle long running searches

ui <- bslib::page_navbar(
  title   = "CopernicusBundle",
  sidebar = bslib::sidebar(
    "TODO"
  ),
  bslib::nav_panel("CopernicusClimate",   climatePageUI("climate"),
                   icon = shiny::tags$img(src = "logo-climate.png", height = "24px", style = "margin-right: 5px;")),
  bslib::nav_panel("CopernicusDataspace", dataspacePageUI("dataspace"),
                   icon = shiny::tags$img(src = "logo-dataspace.png", height = "24px", style = "margin-right: 5px;")),
  bslib::nav_panel("CopernicusMarine",    marinePageUI("marine"),
                   icon = shiny::tags$img(src = "logo-marine.png", height = "24px", style = "margin-right: 5px;")),
  lang    = "en-GB",
  bslib::nav_spacer(),
  authenticationUI("mod_auth")
)

server <- function(input, output, session) {
  auth      <- authenticationServer("mod_auth")
  climate   <- climatePageServer("climate")
  dataspace <- dataspacePageServer("dataspace")
  marine    <- marinePageServer("marine")
  
  shiny::observe({ auth() })
  shiny::observe({ climate() })
  shiny::observe({ dataspace() })
  shiny::observe({ marine() })
}

shiny::shinyApp(ui, server)