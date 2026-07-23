climateRequestUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_column_wrap(
    bslib::card(
      full_screen = TRUE,
      bslib::card_title("R Code"),
      shiny::verbatimTextOutput(ns("txtRequest"))
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_title("Request status"),
      shiny::verbatimTextOutput(ns("txtStatus")),
      shiny::actionButton(ns("btnSubmit"), "Submit request")
    )
  )
}

climateRequestServer <- function(id, request_form) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      latest_job <- shiny::reactiveVal()
      
      output$txtRequest <- shiny::renderText({
        rf <- request_form()
        if (is.null(rf)) {
          "Compose request in 'prepare' tab and click 'request' first"
        } else {
          deparse(rf) |> paste(collapse = "\n")
        }
      })
      
      costs <- shiny::reactive({
        rf <- request_form()
        if (is.null(rf)) {
          NULL
        } else {
          tryCatch({
            CopernicusClimate::cds_estimate_costs(rf)
          }, error = \(e) NULL)
        }
      })
      
      output$txtStatus <- shiny::renderText({
        deparse(costs())
      })
      
      shiny::observeEvent(
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
            
            shiny::modalDialog( msg, title = "Warning") |>
              shiny::showModal()
            
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