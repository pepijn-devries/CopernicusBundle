marineAccountUI <- function(id) {
  ns <- NS(id)
  tagList(
    "TODO"
  )
}

marineAccountServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return(reactive({ }))
    }
  )
}