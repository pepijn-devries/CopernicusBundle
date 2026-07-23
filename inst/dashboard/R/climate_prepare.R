climatePrepareUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::toolbar(
      gap = 5,
      shiny::actionButton(ns("btnRequest"), "Prepare Request"),
      shiny::textOutput(ns("txtProduct"))
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
        shiny::uiOutput(ns("subsetUI"))
      )
    )
  )
}

climatePrepareServer <- function(id, product) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    request <- shiny::reactiveVal(NULL)
    bbox_mod <- geoboxServer("queryBbox")
    
    form <- shiny::reactive({
      prod <- product()
      if (length(prod) == 0) {
        NULL
      } else {
        list(
          product = prod,
          form = CopernicusClimate::cds_dataset_form(prod$id)
        )
      }
    })
    
    output$txtProduct <- shiny::renderText({
      pr <- form()$product$id
      if (is.null(pr)) {
        "Select a product in the 'search' tab first"
      } else {
        pr
      }
    })
    
    output$subsetUI <- shiny::renderUI({
      prod <- form()$product
      fm   <- form()$form
      if (length(prod) == 0) {
        "Select a product in 'search' tab first"
      } else {
        ext <- prod$extent[[1]]$temporal$interval |>
          unlist() |>
          lubridate::as_datetime()

        widgets <-
          fm |>
          dplyr::rowwise() |>
          dplyr::mutate(
            widget = list({
              det <- .data$details$details
              ## area is handled with leaflet widget:
              if (is.na(.data$id) && .data$name != "licences") {
                NULL
              } else {
                widget_name <- ns(.data$name)
                switch(
                  .data$type,
                  DateRangeWidget = {
                    shinyWidgets::airDatepickerInput(
                      widget_name,
                      range = TRUE,
                      value = c(det$defaultStart, det$defaultEnd),
                      minDate = det$minStart,
                      maxDate = det$maxEnd
                    )
                  },
                  StringListWidget = {
                    shiny::selectInput(
                      widget_name,
                      .data$label,
                      choices = setNames(names(det$labels), det$labels),
                      selected = det$default,
                      multiple = TRUE
                    )
                  },
                  StringChoiceWidget = {
                    nm <- names(det$values)
                    if (is.null(nm)) nm <- det$values
                    shiny::selectInput(
                      widget_name,
                      .data$label,
                      choices = setNames(nm, det$values),
                      multiple = FALSE,
                      selected = det$default[[1]]
                    )
                  },
                  StringListArrayWidget = {
                    if (det$accordionGroups) {
                      dat <-
                        det$groups |> lapply(tidyr::as_tibble) |>
                        dplyr::bind_rows() |>
                        tidyr::unnest(c("values", "labels")) |>
                        dplyr::group_by(dplyr::across("label")) |>
                        dplyr::summarise(
                          widget = list(setNames(.data$values, .data$labels)),
                          .groups = "keep"
                        ) |>
                        dplyr::ungroup() |>
                        dplyr::summarise(
                          all = list({
                            setNames(.data$widget,
                                     .data$label)
                          }
                          )) |>
                        dplyr::pull("all")
                      shinyWidgets::virtualSelectInput(
                        widget_name,
                        .data$name,
                        dat[[1]],
                        multiple = TRUE,
                        showValueAsTags = TRUE,
                        optionsCount = 5,
                        search = TRUE
                      )
                      
                    } else {
                      "Widget not implemented, please file an issue report"
                    }
                  },
                  ExclusiveGroupWidget = {
                    ## TODO this is a strange input field
                    shiny::textInput(
                      widget_name,
                      .data$label,
                      value = det$default
                    )
                  },
                  LicenceWidget = {
                    dat <-
                      det$licences |>
                      lapply(as.data.frame) |>
                      dplyr::bind_rows() |>
                      dplyr::mutate(
                        contents_url = {
                          lapply(.data$contents_url, \(cu) {
                            tryCatch({
                              md <- tempfile(fileext = ".md")
                              download.file(cu, md, quiet = TRUE)
                              litedown::mark(md, NA)
                            }, error = \(e) "Could not render license text")
                          })
                        },
                        spdx_identifier = 
                          sprintf("<a href='%s'>%s</a>",
                                  .data$attachment_url,
                                  .data$spdx_identifier)
                      ) |>
                      dplyr::select(!tidyr::any_of("attachment_url")) |>
                      dplyr::rename(contents = "contents_url")
                    shiny::tagList(
                      shiny::tags$label(
                        "Licences",
                        class="control-label"
                      ),
                      DT::datatable(
                        dat,
                        rownames = FALSE,
                        selection = "none",
                        options = list(
                          searching = FALSE,
                          paging = FALSE
                        ),
                        escape = -which(
                          names(dat) %in% c("contents",
                                            "spdx_identifier")))
                    )
                  },
                  FreeEditionWidget = {
                    ## TODO check if we need this widget
                    NULL
                  },
                  GeographicLocationWidget = {
                    NULL ## TODO Not handled yet
                  },
                  GeographicExtentWidget = {
                    NULL ## Handled in separate panel with leaflet
                  },
                  ExclusiveGroupAccordionWidget = {
                    ## Again a strange input field
                    shiny::textInput(
                      widget_name,
                      .data$label,
                      value = det$default
                    )
                  },
                  {
                    sprintf("'%s' not implemented, please report", .data$type)
                  }
                )
              }
            })
          ) |>
          dplyr::pull(widget)
        do.call(shiny::tagList, widgets)
      }
    })
    
    shiny::observeEvent(input$btnRequest, {
      pr <- form()$product$id
      if (is.null(pr)) {
        shiny::modalDialog(
          "Select a product in the 'search' tab first",
          title = "Warning!",
          easyClose = TRUE
        ) |>
          shiny::showModal()
        return()
      }
      req <- CopernicusClimate::cds_build_request(pr)
      fm <- form()$form
      for (element in names(req)) {
        if (!is.null(input[[element]])) {
          ## TODO use list depending on input type
          new_val <- input[[element]]
          idx <- which(fm$name == element)
          if (fm$type[[idx]] %in%
              c("StringListWidget", "StringListArrayWidget")) {
            new_val <- as.list(new_val)
          } else if (fm$type[[idx]] == "DateRangeWidget") {
            new_val <- as.list(paste(
              format.Date(new_val, "%Y-%m-%d"),
              collapse = "/"
            ))
          }
          req[[element]] <- new_val
        }
      }
      idx <- which(fm$type == "GeographicExtentWidget")
      if (length(idx) > 0 && !("GeographicLocationWidget" %in% fm$type)) {
        bbox <- bbox_mod() |> setNames(c("e", "s", "w", "n"))
        req[["area"]] <- bbox[c("n", "w", "s", "e")]
      }
      request(req)
    })
    
    return(request)
  })
}