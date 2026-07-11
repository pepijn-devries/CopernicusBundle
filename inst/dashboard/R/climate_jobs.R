climateJobsUI <- function(id) {
  ns <- NS(id)
  tagList(
    "TODO",
    actionButton(ns("btnUpdate"), "Update jobs"),
    downloadButton(ns("btnDownload"), "Download job"),
    actionButton(ns("btnRemove"), "Remove job"),
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
        get_jobs()$jobID[sel]
      })
      
      output$btnDownload <- downloadHandler(\() {
        job <- get_selected_jobs()
        details <- tryCatch({
          CopernicusClimate::cds_job_results(job)
        }, error = \(e) {
          showModal(modalDialog(paste(e$body, collapse = " - ")))
          NULL
        })
        if (is.null(details)) req(FALSE)
        tempfile() #TODO
      }, \(file){
        browser() #TODO
        con <- file(file, "wb")
        close(con)
      })

      observeEvent(input$btnRemove, {
        CopernicusClimate::cds_delete_job(
          get_selected_jobs()
        )
        job_update(job_update() + 1)
      })
      
      output$dtJobs <- DT::renderDataTable({
        DT::datatable({
          dat <- get_jobs()
          dat <-
            dat |>
            mutate(
              across(
                any_of(c("created", "started", "finished", "updated")), ~
                {
                  difft <- Sys.time() - lubridate::as_datetime(.x)
                  sprintf("%.1f %s", as.numeric(difft), attr(difft, "units"))
                })
            )
          if (nrow(dat) == 0)
            dat <- data.frame(`no jobs to show` = integer(), check.names = FALSE)
          dat
        })
      })
      
      return(reactive({ }))
    }
  )
}