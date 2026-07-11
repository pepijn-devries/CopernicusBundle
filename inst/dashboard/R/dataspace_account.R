dataspaceAccountUI <- function(id) {
  ns <- NS(id)
  tagList(
    "TODO"
  )
}

dataspaceAccountServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return(reactive({ }))
    }
  )
}