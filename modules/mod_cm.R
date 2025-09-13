# Page for cm visuals
# pass in data needed from the server

mod_cm_ui <- function() {
  navbarPage(
    title = "CM Visuals",
    tabPanel("Donut Chart",
      tagList(
        # CSS for donut chart makes it not wrap and keeps the donut charts side by side
        tags$style(HTML("
          .med-grid {
            display: grid;
            grid-template-columns: 380px 1fr;
            gap: 16px;
            align-items: start;
            width: 100%;
            overflow: hidden;    
            box-sizing: border-box;
          }
          
          .med-card {
              background: #fff;
              border: 1px solid #e5e7eb;
              border-radius: 12px;
              box-shadow: 0 1px 3px rgba(0,0,0,0.05);
              padding: 14px;
              max-width: 100%; 
              box-sizing: border-box;
          }
          
          @media (max-width: 1100px) {
            .med-grid { grid-template-columns: 1fr; }
          }
          
          .selectize-dropdown .selectize-dropdown-content {
            max-height: 260px; overflow-y: auto;
          }
          
          .med-title { margin: 0 0 10px 0; font-weight: 600; font-size: 16px; }
          
          .split-summary {
              font-size: 15px;
              line-height: 1.4;
              margin-top: 10px;
          }
        ")),
        div(class = "med-card",
          div(class = "med-title", "By Medication"),
          div(class = "med-grid",
              # LEFT: filter
              div(
                tags$h4("Filter"),
                selectizeInput(
                  inputId = "cmtrt_select",
                  label   = "Medication (CMTRT)",
                  choices = NULL,   # populated in server
                  selected = NULL,
                  multiple = FALSE,
                  options = list(
                    placeholder = "Type to search medications…",
                    openOnFocus = TRUE
                  )
                ),
                helpText("Only relevent meds (taken after trial start) are displayed"),
                hr(),
                # Donut chart breakdown (under filter)
                htmlOutput("donut_split_summary") 
              ),
              # RIGHT: donut
              div(
                style = "max-width:100%; overflow:hidden;",
                plotOutput("donut_by_med", height = "380px")
              )
          )
        )
      )
    ),
    tabPanel("Patient Profile", "")
  )
}


mod_cm_server <- function(dm_r, cm_r, output, input) {
  dm <- reactive({
    req(dm_r())
    validate(need(is.data.frame(dm_r()), "DM must be a data.frame"))
    dm_r()
  })
  
  # All the unique USUBJID's arms (no need for subject failure) added start and end dates
  dm_arm <- reactive({
    dm() %>% 
      dplyr::filter(!is.na(RFSTDTC)) %>%
      dplyr::distinct(USUBJID, ARM, ARMCD, RFSTDTC, RFENDTC)
  })
  
  
  cm <- reactive({
    req(cm_r())
    validate(need(is.data.frame(cm_r()), "CM must be a data.frame"))
    cm_r()
  })
  
  # Add the ARMS and the dates of the start and end
  cm_by_arm <- reactive({
    cm() %>%
      dplyr::left_join(dm_arm(), by = "USUBJID")
  })
  
  # The total subjects in each arm
  arm_totals <- reactive({
    dm_arm() %>%
      dplyr::group_by(ARM) %>%
      dplyr::summarise(
      total_subjects = dplyr::n_distinct(USUBJID, na.rm =TRUE),
      .groups = "drop"
      )
  })
  
  # ===================================================
  # ======== DONUT CHART WITH FILTER BACK-END =========
  # ===================================================
  # Keep only CM rows with CM start >= RFSTDTC
  cm_valid <- reactive({
    df <- cm_by_arm()
    df$CMSTDTC_parsed <- suppressWarnings(lubridate::ymd(df$CMSTDTC))
    df$RFSTDTC_parsed <- suppressWarnings(lubridate::ymd(df$RFSTDTC))
    df %>%
      dplyr::filter(
        !is.na(CMSTDTC_parsed), !is.na(RFSTDTC_parsed),
        CMSTDTC_parsed >= RFSTDTC_parsed
      )
  })
  
  # create the searchable med list from valid rows only
  observe({
    meds <- cm_valid() %>%
      dplyr::distinct(CMTRT) %>%
      dplyr::arrange(CMTRT) %>%
      dplyr::pull(CMTRT)
    updateSelectizeInput(
      inputId = "cmtrt_select",
      choices = meds,
      selected = if (length(meds)) meds[[1]] else NULL,
      server   = TRUE
    )
  })
  
  # count by ARM for the selected medication (include zero slices for missing)
  cm_by_med_counts <- reactive({
    req(input$cmtrt_select)
    counts <- cm_valid() %>%
      dplyr::filter(CMTRT == input$cmtrt_select) %>%
      dplyr::group_by(ARM) %>%
      dplyr::summarise(n_subjects = dplyr::n_distinct(USUBJID, na.rm = TRUE),
                       .groups = "drop")
    
    all_arms <- dm_arm() %>% dplyr::distinct(ARM)
    all_arms %>%
      dplyr::left_join(counts, by = "ARM") %>%
      dplyr::mutate(n_subjects = tidyr::replace_na(n_subjects, 0L))
  })
  
  # One donut: slices are ARMs for the chosen med
  output$donut_by_med <- renderPlot({
    df <- cm_by_med_counts()
    req(nrow(df) > 0)
    
    total_n <- sum(df$n_subjects)
    if (total_n == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(
            title = paste0("No subjects used: ", input$cmtrt_select, " (after RFSTDTC)"),
            x = NULL, y = NULL
          ) +
          ggplot2::theme_void(base_size = 13) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
      )
    }
    
    # percent for info/labels if needed later
    df <- df %>%
      dplyr::mutate(pct = n_subjects / total_n)
    
    # stable palette across ARMs
    arms <- sort(unique(df$ARM))
    pal  <- stats::setNames(
      grDevices::hcl.colors(max(3, length(arms)), "Set2")[seq_along(arms)],
      arms
    )
    
    ggplot2::ggplot(df, ggplot2::aes(x = 2, y = n_subjects, fill = ARM)) +
      ggplot2::geom_col(width = 1, color = "white") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::scale_fill_manual(values = pal, drop = FALSE) +
      ggplot2::xlim(0.5, 2.5) +
      ggplot2::labs(
        title = paste0("Usage by ARM for: ", input$cmtrt_select),
        fill  = "ARM"
      ) +
      ggplot2::theme_void(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", color = "black"),
        legend.position = "right",
        legend.title = ggplot2::element_text(color = "black"),
        legend.text  = ggplot2::element_text(color = "black")
      )
  })
  
  # ===================================================
  # ============ DONUT BREAKDOWN TABLE ================
  # ===================================================
  # Percentages by ARM for the selected medication
  split_summary <- reactive({
    req(cm_by_med_counts())
    df <- cm_by_med_counts()
    total <- sum(df$n_subjects)
    if (total == 0) {
      return("No subjects used this medication.")
    }
    df %>%
      dplyr::mutate(
        pct = round(100 * n_subjects / total, 1)
      ) %>%
      dplyr::arrange(desc(n_subjects))
  })
  
  # Render nicely formatted text/HTML
  output$donut_split_summary <- renderUI({
    df <- split_summary()
    # if it's the "no subjects" message
    if (is.character(df)) {
      return(tags$p(df))
    }
    # HTML styled table for easier readability
    tbl <- tags$table(
      style = "width:100%; border-collapse: collapse;",
      tags$thead(
        tags$tr(
          tags$th("ARM", style="text-align:left; padding:4px; border-bottom:1px solid #ccc;"),
          tags$th("Subjects", style="text-align:right; padding:4px; border-bottom:1px solid #ccc;"),
          tags$th("Percent", style="text-align:right; padding:4px; border-bottom:1px solid #ccc;")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$ARM[i], style="padding:4px;"),
            tags$td(df$n_subjects[i], style="padding:4px; text-align:right;"),
            tags$td(paste0(df$pct[i], "%"), style="padding:4px; text-align:right;")
          )
        })
      )
    )
    
    div(class="split-summary",
        tags$h5("Chart Split"),
        tbl
    )
  })
}