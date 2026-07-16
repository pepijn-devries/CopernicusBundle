dataspaceSearchResultUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("dtSearchResults"))
  )
}

dataspaceSearchResultServer <- function(id, search) {
  moduleServer(
    id,
    function(input, output, session) {
      search_result <- reactive({
        srch <- search()
        if (is.null(srch)) return()
        bbox <- srch$bbox |>
          setNames(c("xmin", "ymin", "xmax", "ymax")) |>
          sf::st_bbox(crs = 4326)
        req <-
          CopernicusDataspace::dse_stac_search_request(srch$collection.id) |>
          slice_head(n = srch$limit) |>
          sf::st_intersects(bbox)
        
        filters <- setdiff(names(srch), c("collection.id", "bbox", "limit"))
        for (ft in filters) {
          val1 <- srch[[ft]][[1]]
          val2 <- srch[[ft]][[2]]
          if ((is.null(val1) || is.na(val1)) && (is.null(val2) || is.na(val2)))
            next
          if ((is.null(val1) || is.na(val1))) {
            req <- req |>
              filter(!!ft <= !!val2)
          } else if ((is.null(val1) || is.na(val1))) {
            req <- req |>
              filter(!!ft >= !!val1)
          } else {
            req <- req |>
              filter(!!ft >= !!val1 & !!ft <= !!val2)
          }
        }
        search_results <- tryCatch({
          res <- req |> collect()
          if (nrow(res) == 0)
            res <- data.frame(`Zero search results` = integer(), check.names = FALSE)
          res
        }, error = \(e) data.frame(`Failed to collect search results` = integer(),
                                   check.names = FALSE))
      })
      
      output$dtSearchResults <- DT::renderDataTable({
        dat <-
          search_result()
        DT::datatable({
          dat
        },
        rownames = FALSE,
        selection = "single")
      })
      
      return(search_result)
    }
  )
}