zarrUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::actionButton(ns("btnDownload"), "Download"),
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
      
      observeEvent(input$btnDownload, {
        subs <- get_proxy_subset()
        numthr <- Sys.getenv("GDAL_NUM_THREADS")
        Sys.setenv(GDAL_NUM_THREADS = "ALL_CPUS")
        Sys.setenv(GDAL_HTTP_MULTICURL = "YES")
        Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
        tryCatch({
          
          # TODO dimensions get lost! See how CopernicusMarine package handles this
          result <- stars::st_as_stars(subs)
          for (dn in dimnames(subs)) {
            old <- stars::st_get_dimension_values(subs, dn)
            new <- stars::st_get_dimension_values(result, dn)
            if (length(intersect(class(new), class(old))) == 0) {
              result <-
                stars::st_set_dimensions(result, dn, values = old)
            }
          }
          result
        },
        error = \(e) NULL,
        finally = {
          Sys.setenv(GDAL_NUM_THREADS = numthr)
        })
      })
      
      get_proxy <- shiny::reactive({
        ast <- asset()
        if (is.null(ast)) return(NULL)
        href <- ast$layer$assets[[1]][[ast$asset]]$href
        if (!endsWith(toupper(href), ".ZARR")) return(NULL)
        tryCatch({
          CopernicusMarine::cms_zarr_proxy(
            ast$layer$collection,
            ast$layer$id,
            ast$variable,
            ast$asset)
        }, error = \(e) NULL)
      })
      
      get_proxy_subset <- shiny::reactive({
        proxy <- get_proxy()
        if (!is.null(proxy)) {
          dims <- stars::st_dimensions(proxy)
          selection <- list()
          bb <- bbox()
          for (nm in names(dims)) {
            widget_name <- paste0("dim_", nm)
            dim_range <- input[[widget_name]] ## TODO Note that this does not trigger unless the dims change
            dim_vals <- stars::st_get_dimension_values(proxy, nm)
            if (nm == "time") {
              dim_range <- lubridate::as_datetime(dim_vals)
              dim_range <- lubridate::as_datetime(dim_range)
            } else {
              dim_vals <- as.numeric(dim_vals)
            }
            if (is.null(dim_range)) {
              if (nm == "longitude") dim_range <- bb[c(1, 3)]
              if (nm == "latitude") dim_range <- bb[c(2, 4)]
            }
            if (length(dim_range) < 2L) {
              selection[[nm]] <- integer()
            } else {
              selection[[nm]] <-
                which(dim_vals >= dim_range[[1]] & dim_vals <= dim_range[[2]])
            }
          }
          
          if (!(length(selection) == 0 || any(lengths(selection) == 0))) {
            ast <- asset()
            vars <- ast$variable
            if (is.null(vars)) {
              vars <- names(ast$layer$properties[[1]]$`cube:variables`)
            }
            # TODO select statement doesn't work!
            # rlang:::inject(proxy[,!!!selection]) |>
            #   dplyr::select(dplyr::any_of(vars))
            rlang:::inject(proxy[,!!!selection])
          }
        }
      })
      
      output$dimensionUI <- shiny::renderUI({
        proxy <- get_proxy()
        if (!is.null(proxy)) {
          dims <- stars::st_dimensions(proxy)
          ast  <- asset()
          widgets <- list()
          for (nm in names(dims)) {
            if (!(nm %in% c("longitude", "latitude"))) {
              prop <- ast$layer$properties[[1]]$`cube:dimensions`[[nm]]
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
                    
                    extent <-
                      round(extent + c(-1, 1)*10^-digits,
                            digits)
                    shiny::sliderInput(
                      widget_name,
                      nm,
                      value = extent,
                      min = extent[[1]],
                      max = extent[[2]],
                      step = 10^-digits,
                      round = -digits
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