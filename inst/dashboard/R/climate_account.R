climateAccountUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("btnUpdate"), "Update"),
    shiny::textOutput(ns("field")),
    jsTreeR::jstreeOutput(ns("treeAccount"))
  )
}

climateAccountServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      my_tree <- shiny::reactive({
        input$btnUpdate
        jsTreeR::jstree(
          convert_to_jstree(
            CopernicusClimate::cds_get_account()
          )
        )
      })
      
      output$treeAccount <- jsTreeR::renderJstree({
        my_tree()
      })
      
      output$field <- shiny::renderText({
        sel <- input$treeAccount_selected
        if (length(sel) > 0) sel[[1]]$data else "Nothing selected"
      })
      
      return(shiny::reactive({ }))
    }
  )
}