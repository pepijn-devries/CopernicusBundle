dataspaceQueryablesUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("btnSearch"), "Search"),
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
        bslib::card_body(uiOutput(ns("queryUI")))
      )
    )
  )
}

dataspaceQueryablesServer <- function(id, collection) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    bbox_mod <- geoboxServer("queryBbox")
    observe({
      bbox_mod() #TODO
    })
    
    iv_val <- reactiveVal(shinyvalidate::InputValidator$new())
    
    queryables <- reactive({
      cn <- collection()
      
      if (is.null(cn)) {
        NULL
      } else {
        CopernicusDataspace::dse_stac_queryables(
          cn$id
        )
      }
    })
    
    output$queryUI <- renderUI({
      qrb <- queryables()

      if (is.null(qrb)) {
        "Select a collection first"
      } else {
        ## TODO remove old iv rules first, to prevent cluttering?
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
                  selectInput(
                    widget_name, prop$title, prop$enum, multiple = TRUE)
                } else {
                  textInput(
                    widget_name, prop$title, placeholde = prop$description)
                }
              },
              integer = {
                shinyWidgets::numericRangeInput(
                  widget_name, prop$title,
                  value = prop$minimum,
                  step = 1,
                  min = ifelse(is.null(prop$minimum), NA, prop$minimum),
                  max = ifelse(is.null(prop$maximum), NA, prop$maximum)
                )
              },
              number = {
                shinyWidgets::numericRangeInput(
                  widget_name, prop$title,
                  value = c(prop$minimum, prop$maximum),
                  min = ifelse(is.null(prop$minimum), NA, prop$minimum),
                  max = ifelse(is.null(prop$maximum), NA, prop$maximum)
                )
              },
              "Not implemented, please file issue report")
          })
        do.call(tagList, uis)
      }
    })
    
    observeEvent(queryables(), {
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

    observeEvent(input$btnSearch, {
      iv_val()$is_valid()
      bb <- bbox_mod()
      #process queryables such that they can be used in a search
    })
    
    return(reactive({ }))
  })
}