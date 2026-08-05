dataspaceSearchResultUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::dataTableOutput(ns("dtSearchResults"))
  )
}

dataspaceSearchResultServer <- function(id, search) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      search_result <- shiny::ExtendedTask$new(\(srch) {
        if (is.null(srch)) return()
        mirai::mirai({
          bbox <- srch$bbox |>
            setNames(c("xmin", "ymin", "xmax", "ymax")) |>
            sf::st_bbox(crs = 4326)
          req <-
            CopernicusDataspace::dse_stac_search_request(srch$collection.id) |>
            dplyr::slice_head(n = srch$limit) |>
            sf::st_intersects(bbox)
          
          filters <- setdiff(names(srch), c("collection.id", "bbox", "limit"))
          for (ft in filters) {
            val1 <- srch[[ft]][[1]]
            val2 <- srch[[ft]][[2]]
            if ((is.null(val1) || is.na(val1)) && (is.null(val2) || is.na(val2)))
              next
            if ((is.null(val1) || is.na(val1))) {
              req <- req |>
                dplyr::filter(!!ft <= !!val2)
            } else if ((is.null(val1) || is.na(val1))) {
              req <- req |>
                dplyr::filter(!!ft >= !!val1)
            } else {
              req <- req |>
                dplyr::filter(!!ft >= !!val1 & !!ft <= !!val2)
            }
          }
          tryCatch({
            res <- req |> dplyr::collect()
            if (nrow(res) == 0)
              res <- data.frame(`Zero search results` = integer(), check.names = FALSE)
            res
          }, error = \(e) data.frame(`Failed to collect search results` = integer(),
                                     check.names = FALSE))
        })
      })
      
      shiny::observeEvent(search(), {
        srch <- search()
        if (search_result$status() == "running") {
          shiny::modalDialog(
            "Please wait for the previous search to complete, before submitting a new search",
            title = "Warning", easyClose = TRUE) |>
            shiny::showModal()
        } else if (!is.null(srch)) search_result$invoke(srch)
      })
      
      
      output$dtSearchResults <- DT::renderDataTable({
        dat <- if (search_result$status() != "success") {
          dat <- data.frame(a = numeric())
          names(dat) <- sprintf("Search status: %s", search_result$status())
          dat
        } else {
          search_result$result()
        }
        
        DT::datatable({
          dat
        },
        rownames = FALSE,
        selection = "single")
      })
      
      shiny::outputOptions(output, "dtSearchResults", suspendWhenHidden = FALSE)
      return(shiny::reactive({
        if (search_result$status() == "success") {
          search_result$result()
        } else {
          NULL
        }
      }))
    }
  )
}