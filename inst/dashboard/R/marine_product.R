marineProductUI <- function(id) {
  ns <- NS(id)
  tagList(
    "Product TODO",
    DT::DTOutput(ns("product_meta"))
  )
}

marineProductServer <- function(id, product) {
  moduleServer(
    id,
    function(input, output, session) {

      product_meta <- reactive({
        prod <- product()
        if (is.null(prod)) NULL else {
          CopernicusMarine::cms_product_metadata(
            prod$product_id[[1]]
          )
        }
      })
      
      output$product_meta <- DT::renderDT({
        meta <- product_meta()
        if (is.null(meta)) {
          meta <- data.frame(`No product selected` = integer(), check.names = FALSE)
        }
        DT::datatable({
          meta
        }, rownames = FALSE)
      })

      return(reactive({ }))
    }
  )
}