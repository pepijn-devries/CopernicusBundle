climatePrepareUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("subsetUI"))
  )
}

climatePrepareServer <- function(id, product) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      product_form <- reactive({
        prod <- product()
        if (length(prod) == 0) {
          NULL
        } else {
          CopernicusClimate::cds_dataset_form(prod$id)
        }
      })
      
      render_list_widgets <- reactive({
        widgets <- product_forms() |>
          filter(type == StringListWidget) |>
          pull(details)
        browser() #TODO
      })
      
      output$subsetUI <- renderUI({
        prod <- product()
        fm   <- product_form()
        if (length(prod) == 0) {
          "Select a product in 'search' tab first"
        } else {
          ext <- prod$extent[[1]]$temporal$interval |>
            unlist() |>
            lubridate::as_datetime()
          tagList(
            shinyWidgets::airDatepickerInput(
              ns("dateRange"), "Select Time Range",
              timepicker = TRUE, range = TRUE,
              minDate = as.Date(ext[[1]]),
              maxDate = as.Date(ext[[2]]),
              tz = "UTC"),
            leaflet::leafletOutput(ns("my_map"))
          )
        }
      })
      
      output$my_map <- leaflet::renderLeaflet({
        prod <- product() #TODO
        rect <- if (length(prod) > 0) {
          prod$extent[[1]]$spatial$bbox |> unlist()
        } else {
          c(0, -89, 360, 89)
        }
        leaflet::leaflet() |>
          leaflet::addTiles() |>
          leaflet::fitBounds(
            rect[[1]], rect[[2]], rect[[3]], rect[[4]]) |>
          leaflet::addRectangles(
            rect[[1]], rect[[2]], rect[[3]], rect[[4]],
            group = "selection") |>
          leaflet.extras::addDrawToolbar(
            targetGroup = "selection",
            polylineOptions = FALSE,
            polygonOptions = FALSE,
            circleOptions = FALSE,
            markerOptions = FALSE,
            circleMarkerOptions = FALSE,
            singleFeature = TRUE,
            rectangleOptions = leaflet.extras::drawRectangleOptions(
              shapeOptions = leaflet.extras::drawShapeOptions(color = "#007bff", weight = 3)
            ),
            editOptions = leaflet.extras::editToolbarOptions(
              selectedPathOptions = leaflet.extras::selectedPathOptions()
            )
          )
      })

      return(reactive({ }))
    }
  )
}