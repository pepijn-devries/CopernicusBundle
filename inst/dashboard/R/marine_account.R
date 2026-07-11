marineAccountUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("btnUpdate"), "Update"),
    textOutput(ns("field")),
    jsTreeR::jstreeOutput(ns("treeAccount"))
  )
}

marineAccountServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      my_tree <- reactive({
        input$btnUpdate
        jsTreeR::jstree(
          convert_to_jstree(
            CopernicusMarine::cms_login()
          )
        )
      })
      
      output$treeAccount <- jsTreeR::renderJstree({
        my_tree()
      })
      
      output$field <- renderText({
        sel <- input$treeAccount_selected
        if (length(sel) > 0) sel[[1]]$data else "Nothing selected"
      })
      
      return(reactive({ }))
    }
  )
}