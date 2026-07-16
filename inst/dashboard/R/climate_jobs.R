climateJobsUI <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::toolbar(
      gap = 5,
      actionButton(ns("btnUpdate"), "Update jobs"),
      actionButton(ns("btnRemove"), "Remove job")
    ),
    DT::dataTableOutput(ns("dtJobs"))
  )
}

climateJobsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      job_update <- reactiveVal(0)
      
      get_jobs <- reactive({
        input$btnUpdate
        job_update()
        CopernicusClimate::cds_list_jobs()
      })
      
      get_selected_jobs <- reactive({
        sel <- input$dtJobs_rows_selected
        get_jobs()[sel,]
      })
      
      observeEvent(input$btnRemove, {
        CopernicusClimate::cds_delete_job(
          get_selected_jobs()$jobID
        )
        job_update(job_update() + 1)
      })
      
      output$dtJobs <- DT::renderDataTable({
        dat <-
          get_jobs()
        dat <-
          dat |>
          mutate(
            across(
              any_of(c("created", "started", "finished", "updated")), ~
                {
                  lapply(.x, \(z) {
                    difft <- Sys.time() - lubridate::as_datetime(z)
                    sprintf("%.1f %s", as.numeric(difft), attr(difft, "units"))
                  }) |> unlist()
                }),
            file = lapply(.data$metadata, \(md) {
              bind_cols(
                as.data.frame(md[["results"]]) |>
                  select(-any_of(c("title", "type", "status"))),
                as.data.frame(md[["datasetMetadata"]])
              )
            })
          ) |>
          select(-any_of("links")) |>
          unnest("file") |>
          mutate(
            file = ifelse(is.na(.data$asset.value.href),
                          "-",
                          sprintf("<a href='%s'>download</a>",
                                  .data$asset.value.href)),
            asset.value.file.size =
              ifelse(is.na(.data$asset.value.file.size),
                     0, .data$asset.value.file.size),
            asset.value.file.size = lapply(
              .data$asset.value.file.size,
              utils:::format.object_size, units = "auto") |>
              unlist()
          ) |>
          rename(file.size = "asset.value.file.size") |>
          relocate(any_of("file"), .after = any_of("metadata")) |>
          select(-starts_with("asset"), -any_of("metadata"))
        if (nrow(dat) == 0)
          dat <- data.frame(`no jobs to show` = integer(), check.names = FALSE)
        DT::datatable({
          dat
        },
        rownames = FALSE,
        selection = "multiple",
        options = list(
          searching = FALSE,
          paging = FALSE
        ),
        escape = -which(
          names(dat) %in% c("file")))
      })
      
      return(reactive({ }))
    }
  )
}