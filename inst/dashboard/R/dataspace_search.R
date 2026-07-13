dataspaceSearchUI <- function(id) {
  ns <- NS(id)
  tagList(
    dataspaceQueryablesUI(ns("modQry"))
  )
}

dataspaceSearchServer <- function(id, collection) {
  moduleServer(
    id,
    function(input, output, session) {
      query <- dataspaceQueryablesServer("modQry", collection)
      
      observe({ query() }) #TODO
      
      return(reactive({ }))
    }
  )
}