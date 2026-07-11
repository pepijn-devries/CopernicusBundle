library(shiny)
library(dplyr)
library(tidyr)

ui <- bslib::page_navbar(
  title   = "CopernicusBundle",
  sidebar = bslib::sidebar(
    "TODO"
  ),
  bslib::nav_panel("CopernicusClimate", climatePageUI("climate")),
  bslib::nav_panel("CopernicusDataspace", dataspacePageUI("dataspace")),
  bslib::nav_panel("CopernicusMarine", marinePageUI("marine")),
  lang    = "en-GB",
  bslib::nav_spacer(),
  authenticationUI("mod_auth")
)

server <- function(input, output, session) {
  auth <- authenticationServer("mod_auth")
  climate   <- climatePageServer("climate")
  dataspace <- dataspacePageServer("dataspace")
  marine    <- marinePageServer("marine")
  
  observe({ auth() })
  observe({ climate() })
  observe({ dataspace() })
  observe({ marine() })
}

shinyApp(ui, server)