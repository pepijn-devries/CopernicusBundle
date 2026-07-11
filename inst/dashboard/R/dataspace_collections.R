dataspaceCollectionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    "TODO",
    DT::DTOutput(ns("collections"))
  )
}

dataspaceCollectionsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$collections <- DT::renderDT({
        DT::datatable({
          CopernicusDataspace::dse_stac_collections()
        })
      })
      
      return(reactive({ }))
    }
  )
}