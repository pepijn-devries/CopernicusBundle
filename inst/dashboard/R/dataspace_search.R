dataspaceSearchUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::toolbar(
      gap = 5,
      shiny::textOutput(ns("txtCollection")),
      shiny::actionButton(ns("btnUpdate"), "Update Queryables"),
      shiny::numericInput(ns("numLimit"), "Limit", 10L, 1L, 1000L, 1L),
      shiny::actionButton(ns("btnSearch"), "Search")
    ),
    bslib::layout_column_wrap(
      bslib::card(
        full_screen = TRUE,
        bslib::card_title("Select Area"),
        bslib::card_body(
          geoboxUI(ns("queryBbox")) # TODO hide/unhide based on queryables
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_title("Filters"),
        bslib::card_body(shiny::uiOutput(ns("queryUI")))
      )
    )
  )
}

dataspaceSearchServer <- function(id, collection) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    iv_val <- shiny::reactiveVal(shinyvalidate::InputValidator$new())
    query <- shiny::reactiveVal()
    
    bbox_mod <- geoboxServer("queryBbox")

    output$txtCollection <- shiny::renderText({
      cn <- collection()
      if (is.null(cn)) "Select a collection first from 'collections' tab" else {
        paste("Collection:", cn$id)
      }
    })
    
    queryables <- shiny::reactive({
      cn <- collection()
      input$btnUpdate
      
      if (is.null(cn)) {
        NULL
      } else {
        tryCatch({
          CopernicusDataspace::dse_stac_queryables(
            cn$id
          )
        }, error = \(e) NULL)
      }
    })
    
    output$queryUI <- shiny::renderUI({
      qrb <- queryables()
      
      if (is.null(qrb)) {
        "Select a collection first"
      } else {
        uis <-
          lapply(names(qrb$properties), \(p_name) {
            prop <- qrb$properties[[p_name]]
            if (is.null(prop$type)) return(NULL)
            widget_name <- ns(gsub("[:]", "-", p_name))
            switch(
              prop$type,
              string = {
                if (!is.null(prop$format) && prop$format == "date-time") {
                  shinyWidgets::airDatepickerInput(
                    widget_name, prop$title, timepicker = TRUE, range = TRUE, tz = "UTC")
                } else if (!is.null(prop$enum)) {
                  shiny::selectInput(
                    widget_name, prop$title, prop$enum, multiple = TRUE)
                } else {
                  shiny::textInput(
                    widget_name, prop$title, placeholde = prop$description)
                }
              },
              integer = {
                shinyWidgets::numericRangeInput(
                  widget_name, prop$title,
                  value = c(
                    ifelse(is.null(prop$minimum), NA, prop$minimum),
                    ifelse(is.null(prop$maximum), NA, prop$maximum)),
                  step = 1,
                  min = ifelse(is.null(prop$minimum), NA, prop$minimum),
                  max = ifelse(is.null(prop$maximum), NA, prop$maximum)
                )
              },
              number = {
                shinyWidgets::numericRangeInput(
                  widget_name, prop$title,
                  value = c(
                    ifelse(is.null(prop$minimum), NA, prop$minimum),
                    ifelse(is.null(prop$maximum), NA, prop$maximum)),
                  min = ifelse(is.null(prop$minimum), NA, prop$minimum),
                  max = ifelse(is.null(prop$maximum), NA, prop$maximum)
                )
              },
              "Not implemented, please file issue report")
          })
        do.call(shiny::tagList, uis)
      }
    })
    
    shiny::observeEvent(queryables(), {
      qrb <- queryables()
      if (is.null(qrb)) return()
      iv_val()$disable() 
      new_iv <- shinyvalidate::InputValidator$new()
      lapply(names(qrb$properties), \(p_name) {
        prop <- qrb$properties[[p_name]]
        if (!is.null(prop$type) && prop$type == "string" &&
            !is.null(prop$pattern) &&
            !(!is.null(prop$format) && prop$format == "date-time")) {
          widget_name <- gsub("[:]", "-", p_name)
          
          new_iv$add_rule(
            widget_name,
            shinyvalidate::compose_rules(
              shinyvalidate::sv_optional(),
              shinyvalidate::sv_regex(
                prop$pattern, "Please check documentation for correct format", perl = TRUE)
            )
          )
        }
      })
      
      iv_val(new_iv)
      iv_val()$enable()
    })
    
    shiny::observeEvent(input$btnSearch, {
      iv_val()$is_valid()
      qrb <- queryables()
      if (is.null(qrb)) {
        shiny::modalDialog(
          "Please select a collection from the `collections` tab first",
          title = "Warning"
        ) |> shiny::showModal()
        return()
      }
      bb <- bbox_mod()
      request <- list()
      lapply(names(qrb$properties), \(p_name) {
        widget_name <- gsub("[:]", "-", p_name)
        request[[p_name]] <<- input[[widget_name]]
        if (all(as.character(request[[p_name]]) == "")) request[[p_name]] <<- NULL
      })
      request[["bbox"]] <- bb
      request[["collection.id"]] <- collection()$id
      request[["limit"]] <- input$numLimit
      query(request)
    })
    
    return(query)
  })
}