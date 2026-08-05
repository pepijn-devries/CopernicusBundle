#TODO fails when selecting a new product after another

nativeUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_column_wrap(
    width = 0.5,
    bslib::card(
      bslib::card_body(
        full_screen = TRUE,
        bslib::card_header("Native Job"),
        bslib::card_body(
          shiny::uiOutput(ns("nativeJobs")),
          bslib::toolbar(
            shiny::actionButton(ns("btnCancel"), "Cancel"),
            shiny::actionButton(ns("btnPrepare"), "Fetch"),
            shiny::downloadButton(ns("btnDownload"), "Download",
                                  enabled = FALSE)
          ),
          shiny::textOutput(ns("txtFileDetails"))
        )
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("File Tree"),
      bslib::card_body(
        max_height = "300px",
        jsTreeR::jstreeOutput(ns("treeFile"))
      )
    )
  )
}

nativeServer <- function(id, asset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      active_mirai <- shiny::reactiveVal(NULL)
      
      shiny::observe({
        if (nativeTask$status() == "success") {
          shinyjs::enable("btnDownload")
        } else {
          shinyjs::disable("btnDownload")
        }
      })

      shiny::observe({
        if (file_selected()) {
          shinyjs::enable("btnPrepare")
        } else {
          shinyjs::disable("btnPrepare")
        }
      })

      shiny::observe({
        if (nativeTask$status() == "running") {
          shinyjs::enable("btnCancel")
        } else {
          shinyjs::disable("btnCancel")
        }
      })
      
      output$btnDownload <- shiny::downloadHandler(\(){
        if (nativeTask$status() == "success") {
          fl <- nativeTask$result()
          basename(fl$body)
        } else {
          stop("File retrieval failed")
        }
      }, \(file) {
        if (nativeTask$status() == "success") {
          fl <- nativeTask$result()
          file.copy(fl$body, file)
        } else {
          stop("File retrieval failed")
        }
      })

      my_tree <- shiny::reactive({
        type_rules <- list(
          folder = list(
            icon = "glyphicon glyphicon-folder-open"
          ),
          file = list(
            icon = "glyphicon glyphicon-file"
          ),
          empty = list(
            icon = "glyphicon glyphicon-info-sign"
          )
        )
        ast <- asset()
        if (is.null(ast)) {
          jsTreeR::jstree(
            theme = "proton",
            types = type_rules,
            list(list(text = "Select asset first", type = "empty"))
          )
        } else {
          fls <-
            CopernicusMarine::cms_list_native_files(
              ast$layer$collection,
              ast$layer$id) |>
            dplyr::group_by(.data$Key) |>
            dplyr::summarise(
              details = list(dplyr::pick(dplyr::everything())))
          fls <- structure(fls$details, names = fls$Key)
          jsTreeR::jstree(
            types = type_rules,
            selectLeavesOnly = TRUE,
            theme = "proton",
            paths_to_jstree(fls)
          )
        }
      })

      shiny::observeEvent(input$btnCancel, {
        job <- active_mirai()
        if (mirai::is_mirai(job) && mirai::unresolved(job)) {
          mirai::stop_mirai(job)
        }
      })

      nativeTask <- shiny::ExtendedTask$new(\(native_file) {
        m <- mirai::mirai({
          path_out <- unlist(strsplit(native_file$path, "/"))[-1:-2]
          file_out <- path_out[length(path_out)]
          path_out <- do.call(file.path, as.list(utils::head(path_out, -1)))
          path_out <- file.path(td, path_out)
          j <- 0
          while (!dir.exists(path_out)) {
            dir.create(path_out, recursive = TRUE)
            j <- j + 1
            if (j > 10) stop("Failed to create directory for downloaded file")
          }
          cms_s3 <- CopernicusMarine::cms_native_s3(
            endpoint = paste0("https://", native_file$data$extra$base_url[[1]]))
          fl <- file.path(path_out, file_out)
          url <-
            cms_s3$generate_presigned_url(
              "get_object",
              params = list(Bucket = native_file$data$extra$Bucket[[1]],
                            Key = native_file$path)
            )
          httr2::request(url) |>
            httr2::req_perform(fl)
        }, native_file = native_file, td = tempdir())
        active_mirai(m)
        return(m)
      })
      
      output$treeFile <- jsTreeR::renderJstree({
        my_tree()
      })
      
      file_selected <- shiny::reactive({
        sel <- input$treeFile_selected_paths
        !(length(sel) == 0 || is.null(sel[[1]]$data$is_file) ||
            !sel[[1]]$data$is_file)
      })
      
      shiny::observeEvent(input$btnPrepare, {
        sel <- input$treeFile_selected_paths
        if (nativeTask$status() == "running") {
          shiny::modalDialog(
            "Wait for current download job to finish first",
            easyClose = TRUE
          ) |>
            shiny::showModal()
        } else if (file_selected()) {
          if (nativeTask$status() == "success") {
            fl_old <- nativeTask$result()$body |> dirname()
            tryCatch({
              unlink(fl_old, TRUE, TRUE)
            }, error = \(e) NULL)
          }
          nativeTask$invoke(sel[[1]])
        } else {
          shiny::modalDialog(
            "Select a file from tree first",
            easyClose = TRUE
          ) |>
            shiny::showModal()
        }
      })
      
      output$txtFileDetails <- shiny::renderText({
        sel <- input$treeFile_selected
        if (file_selected()) {
          sel <- sel[[1]]
          fz <-
            structure(sel$data$extra$Size[[1]], class = "object_size") |>
            format(units = "auto")
          sprintf("Selected file: size %s; modified %s",
                  fz, sel$data$extra$LastModified[[1]])
        } else {
          "Select file first"
        }
        
      })
      
      output$nativeJobs <- shiny::renderText({
        sprintf("Status of fetching job: '%s'",
          nativeTask$status())
      })
      
      return( shiny::reactive({ }) )
    }
  )
}