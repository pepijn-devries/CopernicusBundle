library(shiny)
library(rlang)

p_i <- getShinyOption("product")
needs_product <- is.null(p_i)

system.file("dashboard", "R", c("climate_prepare.R", "geobox_select.R"),
            package = "CopernicusBundle") |>
  lapply(source)

ui <- fluidPage(
  if (needs_product) textInput("txtProduct", "Product", placeholder = "Product identifier"),
  climatePrepareUI("prepareMod")
)

server <- function(input, output, session) {
  product <- reactive({
    if (needs_product) {
      product_id <- input$txtProduct
    } else {
      product_id <- p_i
    }
    if (product_id == "") return(NULL)
    tryCatch({
      CopernicusClimate::cds_list_datasets(product_id)
    }, error = \(e) NULL)
  })
  
  if (needs_product) {
    validator <- shinyvalidate::InputValidator$new()
    validator$add_rule("txtProduct", \(txt) {
      if (is.null(product()) && txt != "") {
        return("Cannot find product")
      }
    })
    validator$enable()
  }
  
  prepare <- climatePrepareServer("prepareMod", product)
  
  observeEvent(prepare(), {
    shiny::stopApp(prepare())
  })
}

shinyApp(ui, server)