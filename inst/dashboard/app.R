library(shiny)

future::plan(future::multisession) ## In order to handle long running searches

ui <- bslib::page_navbar(
  title   = "CopernicusBundle",
  sidebar = bslib::sidebar(
    "TODO"
  ),
  bslib::nav_panel("CopernicusClimate",   climatePageUI("climate"),
                   icon = tags$img(src = "logo-climate.png", height = "24px", style = "margin-right: 5px;")),
  bslib::nav_panel("CopernicusDataspace", dataspacePageUI("dataspace"),
                   icon = tags$img(src = "logo-dataspace.png", height = "24px", style = "margin-right: 5px;")),
  bslib::nav_panel("CopernicusMarine",    marinePageUI("marine"),
                   icon = tags$img(src = "logo-marine.png", height = "24px", style = "margin-right: 5px;")),
  lang    = "en-GB",
  bslib::nav_spacer(),
  authenticationUI("mod_auth")
)

server <- function(input, output, session) {
  auth      <- authenticationServer("mod_auth")
  climate   <- climatePageServer("climate")
  dataspace <- dataspacePageServer("dataspace")
  marine    <- marinePageServer("marine")
  
  observe({ auth() })
  observe({ climate() })
  observe({ dataspace() })
  observe({ marine() })
}

shinyApp(ui, server)