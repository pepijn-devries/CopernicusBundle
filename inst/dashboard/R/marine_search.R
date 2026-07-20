marineSearchUI <- function(id) {
  ns <- NS(id)
  tab_name <- "marine_data"
  module_css <- sprintf("
    #%s .dataTables_scrollBody { transform: rotateX(180deg); }
    #%s .dataTables_scrollBody table { transform: rotateX(180deg); }
  ", ns(tab_name), ns(tab_name))
  
  tagList(
    tags$head(tags$style(HTML(module_css))),
    bslib::toolbar(
      actionButton(ns("btnUpdate"), "Update product list")
    ),
    DT::DTOutput(ns(tab_name))
  )
}

marineSearchServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      marine_list <- reactive({
        input$btnUpdate
        tryCatch({
          CopernicusMarine::cms_products_list()
        }, error = \(e) data.frame(`Failed to download product list` = integer(),
                                   check.names = FALSE))
      })

      marine_edit <-
        reactive({
          marine_list() |>
            dplyr::mutate(
              dplyr::across(tidyr::any_of("thumbnailUrl"), ~ 
                       sprintf("<img src='%s' width='85px'>",  .x))
            ) |>
            dplyr::rename(tidyr::any_of(c(thumbnail = "thumbnailUrl"))) |>
            dplyr::relocate(tidyr::any_of("thumbnail"))
        })
      
      output$marine_data <- DT::renderDT({
        marine_edit() |>
          DT::datatable(
            options = list(scrollX = TRUE),
            rownames = FALSE,
            selection = "single",
            escape = -which(
              names(marine_edit()) %in% c("thumnail")))

      })
      
      return(reactive({
        sel <- input$marine_data_rows_selected
        if (length(sel) > 0) marine_list()[sel,] else NULL
      }))
    }
  )
}