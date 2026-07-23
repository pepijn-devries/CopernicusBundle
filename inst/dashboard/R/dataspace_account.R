dataspaceAccountUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    "TODO"
  )
}

dataspaceAccountServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      return(shiny::reactive({ }))
    }
  )
}