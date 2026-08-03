wmtsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    "TODO Not yet implemented"
  )
}

wmtsServer <- function(id, asset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      return( shiny::reactive({ }) )
    }
  )
}