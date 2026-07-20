marineProductUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("btnUpdate"), "Update meta info"),
    textOutput(ns("txtProduct")),
    selectInput(ns("selectDataset"), "Dataset", NULL),
    uiOutput(ns("subsetUI"))
  )
}

marineProductServer <- function(id, product) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$txtProduct <- renderText({
        prod <- product()
        if (is.null(prod)) {
          "Please select a product in the 'search' tab"
        } else {
          prod$product_id
        }
      })
      
      product_meta <- reactive({
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

      observeEvent(product_meta(), {
        meta <- product_meta()
        descript <- lapply(meta$properties, \(x) {
          if (is.null(x$admp_title)) "No description" else
            x$admp_title
        }) |>
          unlist()
        if (!is.null(meta)) {
          updateSelectInput(
            "selectDataset",
            choices = meta$id |> setNames(descript),
            selected = meta$id[[1]],
            session = session
          )
        }
      })

      output$subsetUI <- renderUI({
        meta <- product_meta()
        if (is.null(meta) || is.null(input$selectDataset))
          return("Select a dataset first")
        meta <- meta |>
          dplyr::filter(id == input$selectDataset)
        selectInput(
          ns("selectAsset"), "Assets",
          names(meta$assets[[1]]),
          names(meta$assets[[1]])[[1]])
      })
      
      get_asset <- reactive({
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