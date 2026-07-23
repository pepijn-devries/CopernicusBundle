marineProductUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("btnUpdate"), "Update meta info"),
    shiny::textOutput(ns("txtProduct")),
    shiny::uiOutput(ns("datasetUI")),
    shiny::uiOutput(ns("assetUI")),
    shiny::uiOutput(ns("varUI"))
  )
}

marineProductServer <- function(id, product) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$txtProduct <- shiny::renderText({
        prod <- product()
        if (is.null(prod)) {
          "Please select a product in the 'search' tab"
        } else {
          prod$product_id
        }
      })
      
      product_meta <- shiny::reactive({
        input$btnUpdate
        prod <- product()
        if (is.null(prod)) NULL else {
          tryCatch({
            CopernicusMarine::cms_product_metadata(
              prod$product_id
            )
          }, error = \(e) NULL)
        }
      })
      
      get_datasets <- shiny::reactive({
        meta <- product_meta()
        descript <- lapply(meta$properties, \(x) {
          if (is.null(x$admp_title)) "No description" else
            x$admp_title
        }) |>
          unlist()
        setNames(meta$id, descript)
      })
      
      get_layer <- shiny::reactive({
        meta <- product_meta()
        dataset <- input$selectDataset
        if (length(meta) == 0 || is.null(dataset)) return(NULL)
        meta |>
          dplyr::filter(.data$id == dataset)    
      })

      output$datasetUI <- shiny::renderUI({
        sets  <- get_datasets()
        if (length(sets) == 0) return(NULL)
        shiny::selectInput(
          ns("selectDataset"),
          "Data set",
          sets,
          sets[1])
      })
      
      output$assetUI <- shiny::renderUI({
        layer <- get_layer()
        ast   <- names(layer$assets[[1]])
        if (is.null(layer)) return("Select a product first")
        shiny::selectInput(
          ns("selectAsset"), "Assets",
          ast,
          ast[1])
      })
      
      output$varUI <- shiny::renderUI({
        layer <- get_layer()
        if (is.null(layer)) {
          return(NULL)
        } else {
          vars <-
            layer |>
            dplyr::pull("properties") |>
            lapply(\(x) names(x[["cube:variables"]])) |>
            unlist()
          shiny::selectInput(
            ns("selectVariable"),
            "Variable",
            vars,
            multiple = TRUE
          )
        }
      })
      
      get_asset <- shiny::reactive({
        list(
          layer    = get_layer(),
          variable = input$selectVariable,
          asset    = input$selectAsset
        )
      })
      
      return(get_asset)
    }
  )
}