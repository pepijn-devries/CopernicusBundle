climateQueryablesUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::toolbar(
      actionButton(ns("btnRequest"), "Request"),
      textOutput(ns("txtProduct"))
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
        uiOutput(ns("subsetUI"))
      )
    )
  )
}

climateQueryablesServer <- function(id, form) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    request <- reactiveVal(NULL)
    bbox_mod <- geoboxServer("queryBbox")
    
    output$txtProduct <- renderText({
      pr <- form()$product$id
      if (is.null(pr)) {
        "Select a product in the 'search' tab first"
      } else {
        pr
      }
    })
    
    output$subsetUI <- renderUI({
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
          rowwise() |>
          mutate(
            widget = list({
              det <- .data$details$details
              ## area is handled with leaflet widget:
              if (.data$name == "area_group") {
                NULL
              } else {
                widget_name <- ns(.data$name)
                switch(
                  .data$type,
                  StringListWidget = {
                    selectInput(
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
                    selectInput(
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
                        det$groups |> lapply(as_tibble) |>
                        bind_rows() |>
                        unnest(c("values", "labels")) |>
                        group_by(across("label")) |>
                        summarise(
                          widget = list(setNames(.data$values, .data$labels)),
                          .groups = "keep"
                        ) |>
                        ungroup() |>
                        summarise(
                          all = list({
                            setNames(.data$widget,
                                     .data$label)
                          }
                          )) |>
                        pull("all")
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
                    textInput(
                      widget_name,
                      .data$label,
                      value = det$default
                    )
                  },
                  LicenceWidget = {
                    dat <-
                      det$licences |>
                      lapply(as.data.frame) |>
                      bind_rows() |>
                      mutate(
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
                      select(!any_of("attachment_url")) |>
                      rename(contents = "contents_url")
                    tagList(
                      tags$label(
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
                  GeographicExtentWidget = {
                    NULL ## Handled in separate panel with leaflet
                  },
                  {
                    "TODO not implemented"
                  }
                )
              }
            })
          ) |>
          pull(widget)
        do.call(tagList, widgets)
      }
    })
    
    observeEvent(input$btnRequest, {
      pr <- form()$product$id
      if (is.null(pr)) {
        modalDialog(
          "Select a product in the 'search' tab first",
          title = "Warning!",
          easyClose = TRUE
        ) |>
          showModal()
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
          }
          req[[element]] <- new_val
        }
      }
      idx <- which(fm$type == "GeographicExtentWidget")
      if (length(idx) > 0) {
        bbox <- bbox_mod() |> setNames(c("e", "s", "w", "n"))
        req[["area"]] <- bbox[c("n", "w", "s", "e")]
      }
      request(req)
    })
    
    return(request)
  })
}