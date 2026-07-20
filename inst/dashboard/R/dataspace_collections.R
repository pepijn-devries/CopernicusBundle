dataspaceCollectionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("collections"))
  )
}

dataspaceCollectionsServer <- function(id) {
  moduleServer(
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
      
      return(reactive({
        sel <- input$collections_rows_selected
        if (length(sel) > 0)
          collections[sel,] else NULL
      }))
    }
  )
}