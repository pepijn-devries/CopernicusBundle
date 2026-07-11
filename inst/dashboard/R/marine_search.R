marineSearchUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("marine_data")),
    "TODO"
  )
}

marineSearchServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      marine_list <-
        CopernicusMarine::cms_products_list()

      marine_edit <-
        marine_list |>
        mutate(
          across(any_of("thumbnailUrl"), ~ 
              sprintf("<img src='%s' width='85px'>",  .x))
        ) |>
        rename(any_of(c(thumbnail = "thumbnailUrl"))) |>
        relocate(any_of("thumbnail"))
      
      output$marine_data <- DT::renderDT({
        marine_edit |>
          DT::datatable(
            rownames = FALSE,
            selection = "single",
            escape = -which(
              names(marine_edit) %in% c("thumnail")))

      })
      
      return(reactive({
        sel <- input$marine_data_rows_selected
        if (length(sel) > 0) marine_list[sel,] else NULL
      }))
    }
  )
}