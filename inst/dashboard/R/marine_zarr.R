zarrUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::downloadButton(ns("btnDownload"), "Download",
                          enabled = FALSE),
    shiny::uiOutput(ns("dimensionUI")),
    geoboxUI(ns("queryBbox")) # TODO hide/unhide based on queryables
  )
}

zarrServer <- function(id, asset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      geo_extend <- shiny::reactive({
        ## TODO get from layer
        c(-180, -90, 180, 90)
      })
      
      bbox <- geoboxServer("queryBbox")

      shiny::observe({
        ast <- asset()
        if (is.null(ast) || nrow(ast$layer) == 0) {
          shinyjs::disable("btnDownload")
        } else {
          shinyjs::enable("btnDownload")
        }
      })
      
      output$btnDownload <- shiny::downloadHandler(
        filename = \() {
          ast <- asset()
          if (is.null(ast)) "fail.nc" else
            paste0(ast$layer$id, ".nc")
        },
        content = \(file) {
          obj <- get_stars_object()
          CopernicusMarine::cms_write_ncdf(obj, file)
        }
      )

      get_stars_object <- shiny::reactive({
        selection <- get_selection()
        ast <- asset()
        if (!is.null(selection)) {
          args <-
            list(
              product = ast$layer$collection,
              layer = ast$layer$id,
              variable = selection$variables,
              asset = ast$asset
            ) |>
            c(selection$selection)
          result <-
            tryCatch({
              do.call(
                CopernicusMarine::cms_download_subset,
                args)
            }, error = \(e) NULL)
        }
      })

      get_selection <- shiny::reactive({
        ast <- asset()
        if (!is.null(ast)) {
          props <- ast$layer$properties[[1]]$`cube:dimensions`
          selection <- list()
          selection <- list()
          for (nm in names(props)) {
            widget_name <- paste0("dim_", nm)
            val <- input[[widget_name]]
            if (nm == "time") {
              nm <- "timerange"
              selection[[nm]] <- lubridate::as_datetime(val)
            } else if (nm %in% c("longitude", "latitude")) {
              next
            } else if (nm == "elevation") {
              nm <- "verticalrange"
              vals <- props$elevation$values |> unlist()
              
              selection[[nm]] <- lapply(val, \(x) {
                x <- abs(vals - as.numeric(x))
                vals[x == min(x)]
              }) |> unlist()
            } else {
              selection[[nm]] <- as.numeric(val)
            }
          }
          selection[["region"]] <- bbox()
          
          if (!(length(selection) == 0 || any(lengths(selection) == 0))) {
            vars <- ast$variable
            if (is.null(vars)) {
              vars <- names(ast$layer$properties[[1]]$`cube:variables`)
            }
            return(list(
              variables = vars,
              selection = selection
            ))
          } else {
            return(NULL)
          }
        }
      })

      output$dimensionUI <- shiny::renderUI({
        ast <- asset()
        props <- ast$layer$properties[[1]]$`cube:dimensions`
        if (!is.null(ast)) {
          widgets <- list()
          for (nm in names(props)) {
            if (!(nm %in% c("longitude", "latitude"))) {
              prop <- props[[nm]]
              widget_name <- ns(paste0("dim_", nm))
              
              widgets[[nm]] <-
                switch(
                  nm,
                  time = {
                    shinyWidgets::airDatepickerInput(
                      widget_name,
                      nm,
                      timepicker = TRUE,
                      value = unlist(prop$extent),
                      minDate = prop$extent[[1]],
                      maxDate = prop$extent[[2]],
                      range = TRUE
                    )
                  },
                  elevation = {
                    extent <- unlist(prop$extent)
                    digits <- floor(6 - log10(diff(extent))) |>
                      c(0) |> max()
                    
                    vals <- round(unlist(prop$values), digits = digits)
                    shinyWidgets::sliderTextInput(
                      widget_name,
                      nm,
                      vals |> as.character(),
                      range(vals) |> as.character()
                    )
                  },
                  "TODO not implemented"
                )
            }
          }
          do.call(shiny::tagList, widgets)
        }
      })
      
      return(shiny::reactive({ }))
    }
  )
}