climatePrepareUI <- function(id) {
  ns <- NS(id)
  tagList(
    climateQueryablesUI(ns("mod_qry"))
  )
}

climatePrepareServer <- function(id, product) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      product_form <- reactive({
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
      
      mod_qry <- climateQueryablesServer("mod_qry", product_form)
      observe({ mod_qry() }) #TODO
      
      return(reactive({ }))
    }
  )
}