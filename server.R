library(shiny)

server <- function(input, output, session) {
  demo <- list(dm = get_dm(), ds = get_ds(), ae = get_ae(), cm = get_cm())
  study <- reactiveVal(demo)
  source_label <- reactiveVal(NULL)
  import_error <- reactiveVal(NULL)
  upload_generation <- reactiveVal(0L)

  selections <- reactiveValues()

  for (domain in names(sdtm_labels)) {
    local({
      key <- domain
      id <- paste0("sdtm_", key)
      remove_id <- paste0(id, "_remove")
      label <- sdtm_labels[[key]]
      if (key == "dm") {
        label <- tagList(label, span("*", class = "trial-required", `aria-hidden` = "true"),
                         span(" (required)", class = "visually-hidden"))
      }
      picker_id <- reactive({
        generation <- upload_generation()
        if (generation == 0L) id else paste0(id, "_", generation)
      })

      observeEvent(input[[picker_id()]], {
        file <- input[[picker_id()]]
        parsed <- tryCatch(
          read_sdtm_domain(file, key),
          sdtm_import_error = function(error) error
        )
        rejected <- inherits(parsed, "sdtm_import_error")
        selections[[key]] <- list(
          name = paste(file$name, collapse = ", "),
          data = if (!rejected) parsed,
          error = if (rejected) conditionMessage(parsed)
        )
        import_error(NULL)
      })

      observeEvent(input[[remove_id]], {
        selections[[key]] <- NULL
        import_error(NULL)
      })

      output[[paste0(id, "_selection")]] <- renderUI({
        selection <- selections[[key]]
        if (is.null(selection)) {
          return(fileInput(
            picker_id(), label, accept = c(".csv", ".xpt"),
            buttonLabel = "Drop or choose file", width = "100%"
          ))
        }

        div(
          tags$div(class = "trial-file-label", label),
          div(
            class = "trial-file-selection",
            if (is.null(selection$error)) span(selection$name) else tags$s(selection$name),
            actionButton(
              remove_id, "×", class = "trial-file-remove",
              `aria-label` = paste("Remove", toupper(key), "file")
            )
          ),
          if (!is.null(selection$error)) {
            tags$p(class = "trial-import-error", role = "alert", selection$error)
          }
        )
      })
    })
  }

  tables <- reactive({
    domains <- names(sdtm_labels)
    selected <- setNames(lapply(domains, function(domain) selections[[domain]]$data), domains)
    Filter(is.data.frame, selected)
  })

  observe({
    updateActionButton(session, "sdtm_import", disabled = is.null(tables()$dm))
  })

  observeEvent(input$sdtm_import, {
    req(tables()$dm)
    imported <- tryCatch(
      validate_sdtm_study(tables()),
      sdtm_import_error = function(error) error
    )
    if (inherits(imported, "sdtm_import_error")) {
      import_error(conditionMessage(imported))
      return()
    }

    study(imported)
    source_label(paste("Loaded", imported$dm$STUDYID[1]))
    import_error(NULL)
    available <- toupper(names(imported))
    for (domain in toupper(names(sdtm_labels))) {
      if (domain %in% available) {
        showTab("clinical_domain", domain, session = session)
      } else {
        hideTab("clinical_domain", domain, session = session)
      }
    }
    updateTabsetPanel(session, "clinical_domain", selected = "DM")
  })

  observeEvent(input$sdtm_reset, {
    req(source_label())
    for (domain in names(sdtm_labels)) {
      selections[[domain]] <- NULL
      showTab("clinical_domain", toupper(domain), session = session)
    }
    upload_generation(upload_generation() + 1L)
    study(demo)
    source_label(NULL)
    import_error(NULL)
  })

  output$sdtm_loaded <- reactive(!is.null(source_label()))
  outputOptions(output, "sdtm_loaded", suspendWhenHidden = FALSE)

  output$sdtm_status <- renderUI({
    error <- import_error()
    if (!is.null(error)) {
      tags$p(class = "trial-import-error", role = "alert", error)
    } else if (!is.null(source_label())) {
      tags$p(class = "domain-note", role = "status", source_label())
    }
  })

  dm_r <- reactive(study()$dm)
  ds_r <- reactive(req(study()$ds))
  ae_r <- reactive(req(study()$ae))
  cm_r <- reactive(req(study()$cm))

  mod_dm_server(input, output, dm_r, session)
  mod_ds_server(dm_r, ds_r, output, input, session)
  mod_ae_server(input, output, dm_r, ae_r, session)
  mod_cm_server(dm_r, cm_r, output, input, session)
}
