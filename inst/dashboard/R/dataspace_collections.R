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
      collections <- shiny::ExtendedTask$new(\() {
        mirai::mirai({
          result <- NULL
          while (is.null(result)) {
            result <- tryCatch({
              CopernicusDataspace::dse_stac_collections()
            }, error = \(e) {Sys.sleep(5); NULL})
          }
          result
        })
      })
      
      collections$invoke()

      output$collections <- DT::renderDT({
        busy <- data.frame(`Please wait while retrieving collections` = integer(),
                           check.names = FALSE)
        dat <-
          switch(
            collections$status(),
            initial = busy,
            running = busy,
            success = {
              collections$result() |>
                dplyr::mutate(
                  keywords = lapply(.data$keywords, paste, collapse = ", ") |>
                    unlist()) |>
                dplyr::select(tidyr::any_of(c("id", "title", "description", "type", "keywords")))
            },
            data.frame(`Failed to retrieve collections` = integer(),
                       check.names = FALSE)
          )
        DT::datatable({ dat }, rownames = FALSE, selection = "single")
      })
      
      return(shiny::reactive({
        if (collections$status() != "success") return(NULL)
        sel <- input$collections_rows_selected
        if (length(sel) > 0)
          collections$result()[sel,] else NULL
      }))
    }
  )
}