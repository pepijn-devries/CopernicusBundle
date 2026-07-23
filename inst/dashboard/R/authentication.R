authenticationUI <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_menu(
    title = "Authentication",
    align = "right",
    bslib::nav_item(
      shiny::actionButton(ns("TestAuthentication"), "Test Authentication")
    ),
    bslib::nav_item(
      if (requireNamespace("CopernicusClimate")) {
        shiny::passwordInput(
          ns("climate_token"), "Climate API token",
          Sys.getenv("ECMWF_DATASTORES_KEY"))
      } else {
        "Install CopernicusClimate if needed"
      },
      if (requireNamespace("CopernicusDataspace")) {
        shiny::tagList(
          shiny::textInput(
            ns("dse_uid"), "Dataspace username",
            Sys.getenv("CDSE_API_USERNAME")),
          shiny::passwordInput(
            ns("dse_pwd"), "Dataspace password",
            Sys.getenv("CDSE_API_PASSWORD")),
          shiny::textInput(
            ns("dse_clientid"), "Dataspace client id",
            Sys.getenv("CDSE_API_CLIENTID")),
          shiny::passwordInput(
            ns("dse_clientsecret"), "Dataspace client secret",
            Sys.getenv("CDSE_API_CLIENTSECRET")),
          shiny::textInput(
            ns("dse_s3id"), "Dataspace S3 ID",
            Sys.getenv("CDSE_API_S3ID")),
          shiny::passwordInput(
            ns("dse_s3secret"), "Dataspace S3 secret",
            Sys.getenv("CDSE_API_S3SECRET"))
        )
      } else {
        "Install CopernicusDataspace if needed"
      },
      if (requireNamespace("CopernicusMarine")) {
        shiny::tagList(
          shiny::textInput(
            ns("marine_username"), "Marine username",
            Sys.getenv("COPERNICUSMARINE_SERVICE_USERNAME")),
          shiny::passwordInput(
            ns("marine_password"), "Marine password",
            Sys.getenv("COPERNICUSMARINE_SERVICE_PASSWORD"))
        )
      } else {
        "Install CopernicusDataspace if needed"
      }
    )
  )
}

authenticationServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      shiny::observeEvent(input$TestAuthentication, {
        shiny::showModal(
          shiny::modalDialog(
            shiny::tabsetPanel(
              shiny::tabPanel(
                "CopernicusClimate",
                if(requireNamespace("CopernicusClimate")) {
                  shiny::verbatimTextOutput(ns("CCcheck"))
                } else {
                  "Please install package CopernicusClimate first"
                }
              ),
              shiny::tabPanel(
                "CopernicusDataspace",
                if(requireNamespace("CopernicusDataspace")) {
                  shiny::verbatimTextOutput(ns("CDcheck"))
                } else {
                  "Please install package CopernicusDataspace first"
                }
              ),
              shiny::tabPanel(
                "CopernicusMarine",
                if(requireNamespace("CopernicusMarine")) {
                  shiny::verbatimTextOutput(ns("CMcheck"))
                } else {
                  "Please install package CopernicusMarine first"
                }
              )
            ),
            title     = "Test Authentication",
            size      = "xl",
            easyClose = TRUE
          )
        )
      })
      
      output$CCcheck <- shiny::renderText({
        CopernicusClimate::cds_check_authentication() |>
          jsonlite::toJSON(, pretty = TRUE, auto_unbox = TRUE)
      })

      output$CDcheck <- shiny::renderText({
        input$TestAuthentication ## make sure to trigger on button click
        paste(
          tryCatch({
            CopernicusDataspace::dse_public_access_token() |>
              jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
          }, error = \(e) "Public token failed"),
          tryCatch({
            CopernicusDataspace::dse_access_token() |>
              jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
          }, error = \(e) "Private token failed"),
          paste("S3 buckets:", available_buckets()),
          sep = "\n"
        )
      })
      
      output$CMcheck <- shiny::renderText({
        tryCatch({
          CopernicusMarine::cms_login() |>
            jsonlite::toJSON(, pretty = TRUE, auto_unbox = TRUE)
        }, error = \(e) "Login failed")
      })
      
      available_buckets <- function() {
        my_s3 <- CopernicusDataspace::dse_s3()
        buckets <- tryCatch({
          my_s3$list_buckets()$Buckets |>
            lapply(\(x) x$Name) |>
            unlist() |>
            paste(collapse = ", ")
        }, error = \(e) "S3 credentials failed")
      }
 
      shiny::observe({
        if (requireNamespace("CopernicusDataspace")) {
          memoise::forget(CopernicusDataspace::dse_public_access_token)
          memoise::forget(CopernicusDataspace::dse_access_token)
        }
        Sys.setenv(ECMWF_DATASTORES_KEY = input$climate_token)
        Sys.setenv(CDSE_API_USERNAME = input$dse_uid)
        Sys.setenv(CDSE_API_PASSWORD = input$dse_pwd)
        Sys.setenv(CDSE_API_CLIENTID = input$dse_clientid)
        Sys.setenv(CDSE_API_CLIENTSECRET = input$dse_clientsecret)
        Sys.setenv(CDSE_API_S3ID = input$dse_s3id)
        Sys.setenv(CDSE_API_S3SECRET = input$dse_s3secret)
        Sys.setenv(COPERNICUSMARINE_SERVICE_USERNAME = input$marine_username)
        Sys.setenv(COPERNICUSMARINE_SERVICE_PASSWORD = input$marine_password)
      })
      
      return(shiny::reactive({ }))
    }
  )
}