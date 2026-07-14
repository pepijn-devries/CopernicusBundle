climateRequestUI <- function(id) {
  ns <- NS(id)
  bslib::layout_column_wrap(
    bslib::card(
      full_screen = TRUE,
      bslib::card_title("R Code"),
      verbatimTextOutput(ns("txtRequest"))
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_title("Request status"),
      verbatimTextOutput(ns("txtStatus")),
      actionButton(ns("btnSubmit"), "Submit request")
    )
  )
}

climateRequestServer <- function(id, request_form) {
  moduleServer(
    id,
    function(input, output, session) {
      latest_job <- reactiveVal()
      
      output$txtRequest <- renderText({
        rf <- request_form()
        if (is.null(rf)) {
          "Compose request in 'prepare' tab and click 'request' first"
        } else {
          deparse(rf) |> paste(collapse = "\n")
        }
      })
      
      costs <- reactive({
        rf <- request_form()
        if (is.null(rf)) {
          NULL
        } else {
          tryCatch({
            CopernicusClimate::cds_estimate_costs(rf)
          }, error = \(e) NULL)
        }
      })
      
      output$txtStatus <- renderText({
        deparse(costs())
      })
      
      observeEvent(
        input$btnSubmit, {
          cst <- costs()
          msg <- ""
          if (is.null(cst)) {
            msg <- "Prepare a request first"
          } else {
            if (cst$cost > cst$limit) msg <-
                sprintf("Your request costs more (%i) than allowed (%i). Reduce your request size and try again.",
                        cst$cost, cst$limit)
          }
          if (msg != "") {
            
            modalDialog( msg, title = "Warning") |>
              showModal()
            
          } else {
            job_id <- 
              CopernicusClimate::cds_submit_job(
                request_form(), check_quota = FALSE,
                wait = FALSE) |>
              suppressMessages()
            latest_job(job_id)
          }
        })
      
      return(latest_job)
    }
  )
}