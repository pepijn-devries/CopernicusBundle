climateSearchUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::searchInput(
      ns("txtSearch"),
      "Search Copernicus Climate",
      btnSearch = icon("magnifying-glass"), 
      btnReset = icon("xmark")),
    DT::DTOutput(ns("searchResult"))
  )
}

climateSearchServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      search_result <- reactive({
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
          select(-any_of("links")) |>
          unnest(any_of("providers")) |>
          mutate(
            across(any_of("description"),
                   ~sprintf("<a href='#%i'>description</a>", row_number()) |>
                     as.character()),
            across(any_of("keywords"),
                   ~sprintf("<a href='#%i'>keywords</a>", row_number()) |>
                     as.character()),
            across(any_of("assets"), ~ lapply(.x, \(x) {
              if ("thumbnail" %in% unlist(x$roles)) {
                sprintf("<img src='%s' width='85px'>",  x$href)
              } else ""
            }))
          ) |>
          rename(any_of(c(thumbnail = "assets"))) |>
          relocate(any_of("thumbnail")) |>
          relocate(any_of(c("type", "stac_version")),
                          .after = any_of("summaries"))
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
      
      return(reactive({
        rw <- input$searchResult_rows_selected
        if (!is.null(rw) && rw > 0) {
          search_result()[rw,]
        } else NULL
      }))
    }
  )
}