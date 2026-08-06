wmtsUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    bslib::card(
      bslib::card_header("Controls"),
      bslib::card_body(
        shiny::uiOutput(ns("wmts_capabilities"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Map"),
      bslib::card_body(
        leaflet::leafletOutput(ns("wmts"))
      )
    )
  )
}

wmtsServer <- function(id, asset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$wmts <- leaflet::renderLeaflet({
        lr <- get_wmts_layer()
        map <- leaflet::leaflet() |>
          lr()
        
        map
      })
      
      capabilities <- shiny::reactive({
        ast <- asset()
        if (is.null(ast) || nrow(ast$layer) == 0 ||
            length(ast$variable) != 1) {
          NULL
        } else {
          tryCatch({
            CopernicusMarine::cms_wmts_get_capabilities(
              ast$layer$collection[[1]],
              ast$layer$id[[1]],
              ast$variable
            )
          }, error = \(e) NULL)
        }
      })
      
      output$wmts_capabilities <- renderUI({
        caps <- capabilities()
        if (is.null(caps)) {
          "Select a product and a single variable first"
        } else {
          widgets <-
            lapply(caps$Dimension[[1]], \(dm) {
              switch(
                dm$Identifier,
                
                elevation = {
                  vals <- as.numeric(dm$Value)
                  digits <- floor(6 - log10(diff(range(vals)))) |>
                    c(0) |> max()
                  vals <- round(vals, digits = digits)
                  shinyWidgets::sliderTextInput(
                    ns("dim_elevation"),
                    "Elevation [m]",
                    vals |> as.character(),
                    round(as.numeric(dm$Default), digits = digits) |>
                      as.character()
                  )
                },
                
                time = {
                  
                  vals <- dm$Value |> strsplit("/") |> unlist()
                  shinyWidgets::airDatepickerInput(
                    ns("dim_time"),
                    minDate = vals[1],
                    maxDate = vals[2],
                    value = dm$Default,
                    timepicker = TRUE
                  )
                },
                sprintf("Dimension '%s' not implemented", dm$Identifier)
              )
          })
          do.call(shiny::tagList, widgets)
        }
      })

      get_wmts_layer <- shiny::reactive({
        ast <- asset()
        caps <- capabilities()
        tm <- input$dim_time
        el <- input$dim_elevation
        if (is.null(caps) || is.null(ast) || nrow(ast$layer) == 0) {
          I
        } else {
          function(map) {
            dms <- caps$Dimension[[1]]
            for (i in seq_along(dms)) {
              if (dms[[i]]$Identifier == "elevation") {
                if (is.null(el)) {
                  el <- dms[[i]]$Default
                } else {
                  dif <- as.numeric(el) - as.numeric(dms[[i]]$Value)
                  el <- dms[[i]]$Value[dif == min(dif)][[1]]
                }
              } else if (dms[[i]]$Identifier == "time") {
                if (is.null(tm)) tm <- dms[[i]]$Default[[1]]
              }
            }
            CopernicusMarine::addCmsWMTSTiles(
              map,
              ast$layer$collection[[1]],
              variable = ast$variable[[1]],
              ast$layer$id[[1]],
              time = tm,
              elevation = el)
          }
        }
      })
      
      return( shiny::reactive({ }) )
    }
  )
}