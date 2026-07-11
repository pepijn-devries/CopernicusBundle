dataspaceSearchUI <- function(id) {
  ns <- NS(id)
  tagList(
    "TODO"
  )
}

dataspaceSearchServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return(reactive({ }))
    }
  )
}