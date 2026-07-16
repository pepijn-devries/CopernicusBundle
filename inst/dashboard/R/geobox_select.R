geoboxUI <- function(id, ...) {
  ns <- NS(id)
  leaflet::leafletOutput(ns("select_map"), ...)
}

geoboxServer <- function(id, area = \() NULL, decoration = \(x) x) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sel <- reactiveVal(c(-180, -90, 180, 90))
    
    output$select_map <- leaflet::renderLeaflet({
      area() #TODO
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        decoration() |>
        leaflet::fitBounds(
          sel()[[1]], sel()[[2]], sel()[[3]], sel()[[4]]) |>
        leaflet::addRectangles(
          sel()[[1]], sel()[[2]], sel()[[3]], sel()[[4]],
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
            remove = FALSE,
            selectedPathOptions = leaflet.extras::selectedPathOptions()
          )
        )
     })
    
    update_feat <- function(feature) {
      if (is.null(feature)) return()

      coords <-
        feature$geometry$coordinates[[1]] |>
        lapply(as.data.frame, col.names = c("x", "y")) |>
        bind_rows()
      sel(c(min(coords$x), min(coords$y), max(coords$x), max(coords$y)))
    }
    
    observeEvent(input$select_map_shape_click, {
      modalDialog(
        bslib::layout_columns(
          col_widths = c(3, 6, 3, 5, 2, 5, 3, 6, 3),
          div(),
          numericInput(ns("north"), "North", sel()[[4]]),
          div(),
          numericInput(ns("west"), "West", sel()[[1]]),
          div(),
          numericInput(ns("east"), "East", sel()[[3]]),
          div(),
          numericInput(ns("south"), "South", sel()[[2]]),
          div()
        ),
        title = "Refine Selection",
        size = "s",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Dismiss"),
          actionButton(ns("btnAccept"), "Accept")
        )
      ) |>
        showModal()
    })
    
    observeEvent(input$btnAccept, {
      #TODO validate input
      sel(c(input$west, input$south, input$east, input$north))
      removeModal()
    })
    
    observe({
      update_feat(input$select_map_draw_new_feature)
    })

    observe({
      update_feat(input$select_map_draw_edited_features$features[[1]])
    })

    return(sel)
  })
}
