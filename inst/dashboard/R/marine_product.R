marineProductUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    bslib::card(
      full_screen = TRUE,
      bslib::card_title("Select asset"),
      bslib::card_body(
        shiny::actionButton(ns("btnUpdate"), "Update meta info"),
        shiny::textOutput(ns("txtProduct")),
        shiny::uiOutput(ns("datasetUI")),
        shiny::uiOutput(ns("assetUI")),
        shiny::uiOutput(ns("varUI"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_title("Handle asset"),
      bslib::card_body(
        marineAssetUI(ns("assetMod"))
      )
    )
  )
}

marineProductServer <- function(id, product) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      get_asset <- shiny::reactive({
        if (length(get_layer()) == 0 || is.null(input$selectAsset))
          return(NULL)
        list(
          layer    = get_layer(),
          variable = input$selectVariable,
          asset    = input$selectAsset
        )
      })
      
      asset <- marineAssetServer("assetMod", get_asset)
      
      shiny::observe({ asset() })
      
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
        previous_asset <- input$selectAsset %||% "native"
          
        layer <- get_layer()
        if (is.null(layer) || nrow(layer) == 0)
          return("Select a product first")
        ast   <- names(layer$assets[[1]])
        if (!(previous_asset %in% ast))
          previous_asset <- ast[1]
        
        shiny::selectInput(
          ns("selectAsset"), "Assets", ast, previous_asset)
      })
      
      output$varUI <- shiny::renderUI({
        layer <- get_layer()
        if (is.null(layer)) {
          return(NULL)
        } else {
          vars <-
            names(layer$properties[[1]]$`cube:variables`)
          var_name <-
            layer$properties[[1]]$`cube:variables` |>
            lapply(\(x) x$name) |> unlist() |> unname()
          shiny::selectInput(
            ns("selectVariable"),
            "Variable",
            vars |> setNames(var_name),
            multiple = TRUE
          )
        }
      })
      
      return(get_asset)
    }
  )
}