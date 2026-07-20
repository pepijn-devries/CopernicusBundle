geoboxUI <- function(id, ...) {
  ns <- NS(id)
  leaflet::leafletOutput(ns("select_map"), ...)
}

geoboxServer <- function(id, area = \() NULL, decoration = \(x) x) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sel <- reactiveVal(c(-180, -90, 180, 90))
    
    geodialog <- modalDialog(
      bslib::layout_columns(
        col_widths = c(3, 6, 3, 5, 2, 5, 3, 6, 3),
        div(),
        numericInput(ns("north"), "North", 90),
        div(),
        numericInput(ns("west"), "West", -180),
        div(),
        numericInput(ns("east"), "East", 180),
        div(),
        numericInput(ns("south"), "South", -90),
        div()
      ),
      title = "Refine Selection",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Dismiss"),
        actionButton(ns("btnAccept"), "Accept")
      )
    )
    
    is_missing <- \(x) {
      is.null(x) || all(is.na(x))
    }
    validator <- shinyvalidate::InputValidator$new()
    validator$add_rule(
      "north", function(value) {
        if (is_missing(value) || is_missing(input$south)) {
          return("Coordinates cannot be missing")
        } else  if (value <= input$south) {
          return("North value should always be greater then South value")
        } else if ((value - input$south) > 180) {
          return("Difference between North and South should by no more than 180 degrees")
        }
      }
    )
    validator$add_rule(
      "south", function(value) {
        if (is_missing(value) || is_missing(input$north)) {
          return("Coordinates cannot be missing")
        } else if (value >= input$north) {
          return("South value should always be less then South value")
        } else if ((input$north - value) > 180) {
          return("Difference between North and South should by no more than 180 degrees")
        }
      }
    )
    validator$add_rule(
      "east", function(value) {
        if (is_missing(value) || is_missing(input$west)) {
          return("Coordinates cannot be missing")
        } else if (value <= input$west) {
          return("East value should always be greater then West value")
        } else if ((value - input$west) > 360) {
          return("Difference between East and West should by no more than 360 degrees")
        }
      }
    )
    validator$add_rule(
      "west", function(value) {
        if (is_missing(value) || is_missing(input$east)) {
          return("Coordinates cannot be missing")
        } else if (value >= input$east) {
          return("West value should always be less then East value")
        } else if ((input$east - value) > 360) {
          return("Difference between East and West should by no more than 360 degrees")
        }
      }
    )
    validator$enable()
    
    output$select_map <- leaflet::renderLeaflet({
      area() #TODO If range is specified, it should be used in validation
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
        dplyr::bind_rows()
      sel(c(min(coords$x), min(coords$y), max(coords$x), max(coords$y)))
    }
    
    observeEvent(input$select_map_shape_click, {
      showModal(geodialog)
    })
    
    observeEvent(input$btnAccept, {
      if (validator$is_valid()) {
        removeModal()
        sel(c(input$west, input$south, input$east, input$north))
      } else {
        modalDialog("Your input is invalid. It will be ignored",
                    title = "Warning") |>
          showModal()
      }
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
