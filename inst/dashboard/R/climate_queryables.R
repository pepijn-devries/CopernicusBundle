climateQueryablesUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("btnRequest"), "Request"),
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
    
    bbox_mod <- geoboxServer("queryBbox")
    observe({
      bbox_mod() #TODO
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
                    "TODO widget not implemented"
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
                {
                  "TODO not implemented"
                }
              )
            })
          ) |>
          pull(widget)
        do.call(tagList, widgets)
      }
    })
    
    observeEvent(input$btnRequest, {
      browser() #TODO
    })
    
    return(reactive({ })) 
  })
}