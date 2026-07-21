climateSearchUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::searchInput(
      ns("txtSearch"),
      "Search Copernicus Climate",
      btnSearch = shiny::icon("magnifying-glass"), 
      btnReset = shiny::icon("xmark")),
    DT::DTOutput(ns("searchResult"))
  )
}

climateSearchServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      search_result <- shiny::reactive({
        st <- input$txtSearch
        if (st == "") {
          data.frame()
        } else {
          CopernicusClimate::cds_search_datasets(
            st
          )
        }
      })
      
      output$searchResult <- DT::renderDT({
        dat <- search_result() |>
          dplyr::select(-tidyr::any_of("links")) |>
          tidyr::unnest(tidyr::any_of("providers")) |>
          dplyr::mutate(
            dplyr::across(tidyr::any_of("description"),
                   ~sprintf("<a href='#%i'>description</a>", row_number()) |>
                     as.character()),
            dplyr::across(tidyr::any_of("keywords"),
                   ~sprintf("<a href='#%i'>keywords</a>", row_number()) |>
                     as.character()),
            dplyr::across(tidyr::any_of("assets"), ~ lapply(.x, \(x) {
              if ("thumbnail" %in% unlist(x$roles)) {
                sprintf("<img src='%s' width='85px'>",  x$href)
              } else ""
            }))
          ) |>
          dplyr::rename(tidyr::any_of(c(thumbnail = "assets"))) |>
          dplyr::relocate(tidyr::any_of("thumbnail")) |>
          dplyr::relocate(tidyr::any_of(c("type", "stac_version")),
                   .after = tidyr::any_of("summaries"))
        if (nrow(dat) == 0)
          dat <- data.frame(`no results to show` = integer(),
                            check.names = FALSE)
        dat  |>
          DT::datatable(
            rownames = FALSE,
            selection = "single",
            escape = -which(
              names(dat) %in% c("assets",
                                "keywords",
                                "description")))
      })
      
      return(shiny::reactive({
        rw <- input$searchResult_rows_selected
        if (!is.null(rw) && rw > 0) {
          search_result()[rw,]
        } else NULL
      }))
    }
  )
}