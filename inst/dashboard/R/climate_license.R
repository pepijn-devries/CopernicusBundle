climateLicensesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("btnUpdate"), "Update Licenses"),
    DT::dataTableOutput(ns("dtLicenses"))
  )
}

climateLicensesServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$dtLicenses <- DT::renderDataTable({
        input$btnUpdate
        DT::datatable(
          CopernicusClimate::cds_list_licences()
        )
      })
      
      return(shiny::reactive({ }))
    }
  )
}