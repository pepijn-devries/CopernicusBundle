marineProductUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("btnUpdate"), "Update meta info"),
    shiny::textOutput(ns("txtProduct")),
    selectInput(ns("selectDataset"), "Dataset", NULL),
    shiny::uiOutput(ns("subsetUI"))
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

      shiny::observeEvent(product_meta(), {
        meta <- product_meta()
        descript <- lapply(meta$properties, \(x) {
          if (is.null(x$admp_title)) "No description" else
            x$admp_title
        }) |>
          unlist()
        if (!is.null(meta)) {
          shiny::updateSelectInput(
            "selectDataset",
            choices = meta$id |> setNames(descript),
            selected = meta$id[[1]],
            session = session
          )
        }
      })

      output$subsetUI <- shiny::renderUI({
        meta <- product_meta()
        if (is.null(meta) || is.null(input$selectDataset))
          return("Select a dataset first")
        meta <- meta |>
          dplyr::filter(id == input$selectDataset)
        shiny::selectInput(
          ns("selectAsset"), "Assets",
          names(meta$assets[[1]]),
          names(meta$assets[[1]])[[1]])
      })
      
      get_asset <- shiny::reactive({
        meta <- product_meta()
        if (is.null(meta) || is.null(input$selectDataset) ||
            is.null(input$selectAsset))
          return(NULL)
        meta <- meta |>
          dplyr::filter(id == input$selectDataset)
        browser() #TODO
        meta$assets[[1]][[input$selectAsset]]
      })
      
      return(get_asset)
    }
  )
}