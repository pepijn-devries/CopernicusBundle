nativeUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    "TODO Not yet implemented"
  )
}

nativeServer <- function(id, asset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      return( shiny::reactive({ }) )
    }
  )
}