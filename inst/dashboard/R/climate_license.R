climateLicensesUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("btnUpdate"), "Update Licenses"),
    DT::dataTableOutput(ns("dtLicenses"))
  )
}

climateLicensesServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$dtLicenses <- DT::renderDataTable({
        input$btnUpdate
        DT::datatable(
          CopernicusClimate::cds_list_licences()
        )
      })
      
      return(reactive({ }))
    }
  )
}