dataspaceCollectionsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("collections"))
  )
}

dataspaceCollectionsServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      collections <- CopernicusDataspace::dse_stac_collections()
      
      output$collections <- DT::renderDT({
        DT::datatable({
          collections |>
            dplyr::mutate(
              keywords = lapply(.data$keywords, paste, collapse = ", ") |>
                unlist()) |>
            dplyr::select(tidyr::any_of(c("id", "title", "description", "type", "keywords")))
        },
        rownames = FALSE,
        selection = "single")
      })
      
      return(shiny::reactive({
        sel <- input$collections_rows_selected
        if (length(sel) > 0)
          collections[sel,] else NULL
      }))
    }
  )
}