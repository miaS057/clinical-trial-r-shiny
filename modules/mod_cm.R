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
          div(class = "med-title", "Donut Chart"),
          div(class = "med-grid",
              # LEFT: filter
              div(
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
                uiOutput("cm_donut_message"),
                plotOutput("donut_by_med", height = "380px")
              )
          )
        )
      )
    ),
    tabPanel("Patient Profile",
     tagList(
       tags$style(HTML("
          .timeline-grid {
          display: grid;
          grid-template-columns: 1fr;
          row-gap: 16px;
          align-items: start;
          width: 100%;
          }
          
          /* give the outer card extra bottom padding so content doesn't touch the app bottom */
          .med-card { padding-bottom: 28px; }
          
          /* table container (scrolls if long) */
          .timeline-table {
          max-height: 280px;
          overflow: auto;
          border: 1px solid #e5e7eb;
          border-radius: 10px;
          }
          
          /* table look */
          .timeline-table table {
          width: 100%;
          border-collapse: collapse;
          font-size: 14px;
          }
          
          .timeline-table th, .timeline-table td {
          padding: 8px 10px;
          border-bottom: 1px solid #f1f5f9;
          text-align: left;
          white-space: nowrap;
          }
          
          .timeline-table th { 
          position: sticky; top: 0; 
          background: #f8fafc; 
          font-weight: 600; 
          }
          
          .timeline-table tr:last-child td { border-bottom: none; }
          ")),
       div(class = "med-card",
           div(class = "med-title", "Patient CM Timeline"),
           div(class = "timeline-grid",
               # TOP: filter
               div(
                 selectizeInput(
                   "usubj_select", "Subject (USUBJID)",
                   choices = NULL, selected = NULL, multiple = FALSE,
                   options = list(placeholder = "Type to search subjects…", openOnFocus = TRUE)
                 ),
                 helpText("Shows all CM for the selected subject")
               ),
               # MIDDLE: timeline
               div(style = "max-width:100%; overflow:hidden;",
                   uiOutput("cm_timeline_message"),
                   plotOutput("cm_timeline", height = "460px")),
               # BOTTOM: NEW table
               div(class = "timeline-table",
                   htmlOutput("cm_timeline_table"))
           )
       )
     )
    )
  )
}


mod_cm_server <- function(dm_r, cm_r, output, input, session) {
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
  
  # Helper function that will help be more lenient on different date time formats
  parse_date_any <- function(x) {
    s <- suppressWarnings
    d <- s(lubridate::ymd_hms(x, quiet = TRUE))
    d <- dplyr::coalesce(d, s(lubridate::ymd(x, quiet = TRUE)))
    as.Date(d)
  }
  
  # ===================================================
  # ======== DONUT CHART WITH FILTER BACK-END =========
  # ===================================================
  # Keep only CM rows with CM start >= RFSTDTC
  cm_valid <- reactive({
    df <- cm_by_arm()
    
    # Use safe parsing with helper function
    df$CMSTDTC_parsed <- parse_date_any(df$CMSTDTC)
    df$RFSTDTC_parsed <- parse_date_any(df$RFSTDTC)
    
    df %>%
      dplyr::filter(
        !is.na(CMSTDTC_parsed), !is.na(RFSTDTC_parsed),
        CMSTDTC_parsed >= RFSTDTC_parsed
      )
  })
  
  # ============= ConMed Picker (for filter) ================
  observe({
    meds <- cm_valid() %>%
      dplyr::distinct(CMTRT) %>%
      dplyr::arrange(CMTRT) %>%
      dplyr::pull(CMTRT)
    updateSelectizeInput(
      inputId = "cmtrt_select",
      choices = meds,
      selected = if (length(meds)) meds[[1]] else NULL,
      server = TRUE
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
  
  # Creates a message for when a medication is not selected
  output$cm_donut_message <- renderUI({
    if (is.null(input$cmtrt_select) || input$cmtrt_select == "") {
      div(
        style = "padding:20px; font-size:16px; color:#555;
        justify-content: center;
        align-items: center;
        text-align: center;",
        "Please select a Medication (CMTRT) to view the CM donut chart."
      )
    }
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
        fill = "ARM"
      ) +
      ggplot2::theme_void(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", color = "black"),
        legend.position = "right",
        legend.title = ggplot2::element_text(color = "black"),
        legend.text = ggplot2::element_text(color = "black")
      )
  })
  

  # ============ Donut Chart BreakDown Table ================
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
        tags$h5("Number of Patients by ARM"),
        tbl
    )
  })
  
  # ===================================================
  # ===== PATIENT TIMELINE WITH FILTER BACK-END =======
  # ===================================================
  
  # ============= Patient Picker (for filter) ================
  observe({
    subs <- cm_valid() %>%
      dplyr::distinct(USUBJID) %>%
      dplyr::arrange(USUBJID) %>%
      dplyr::pull(USUBJID)
    
    updateSelectizeInput(
      inputId = "usubj_select",
      choices = subs,
      selected = if (length(subs)) subs[[1]] else NULL,
      server = TRUE
    )
  })
  
  # Get the subject info
  subject_info <- reactive({
    req(input$usubj_select)
    dm_arm() %>%
      dplyr::filter(USUBJID == input$usubj_select) %>%
      dplyr::slice(1)  # just in case
  })
  
  # Gets all the CM data even before start date
  cm_all <- reactive({
    cm() %>%
      dplyr::left_join(dm_arm(), by = "USUBJID")
  })
  
  # Get the conmed info for the subject
  cm_subject <- reactive({
    req(input$usubj_select)
    
    # gets all the CM data for one patient
    df <- cm_all() %>%
      dplyr::filter(USUBJID == input$usubj_select)
    
    if (nrow(df) == 0L) return(df[0, ])
    
    # This will be more lenient with the whitespaces and caps of a CM
    df <- df %>%
      dplyr::mutate(
        CMTRT = stringr::str_squish(CMTRT)
      )
    
    # data parsing (safely with helper)
    df$CMSTDTC_parsed <- parse_date_any(df$CMSTDTC)
    df$CMENDTC_parsed <- parse_date_any(df$CMENDTC)
    
    # visible start/end
    df$start_date <- as.Date(df$CMSTDTC_parsed)
    df$end_date <- as.Date(df$CMENDTC_parsed)
    
    # Checks that the end_date is right and solves edge cases
    df$end_date <- dplyr::if_else(
      !is.na(df$end_date) & !is.na(df$start_date) & df$end_date < df$start_date,
      df$start_date,
      df$end_date
    )
    
    # Visible bar end:
    # For rows with an end date, ensure at least one-day width.
    # If end is missing, keep NA here so your plotting code can detect
    # “start-only” and extend to the plot edge.
    df$end_vis <- dplyr::if_else(
      !is.na(df$end_date),
      pmax(df$end_date, df$start_date + lubridate::days(1)),
      as.Date(NA)
    )
    df
  })
  
  # So create the levels (how we are organizing the timeline and the table) here
  # This way we can input it into both the timeline and the table for same ordering
  cm_levels <- reactive({
    cs <- cm_subject()
    if (nrow(cs) == 0L) return(character(0))
    
    # Use the same rule the timeline uses
    cs %>%
      dplyr::filter(!is.na(start_date)) %>%
      dplyr::arrange(start_date, end_vis, CMTRT) %>%
      dplyr::pull(CMTRT) %>%
      unique() %>%
      rev()
  })
  
  # Creates a message for when a subject is not selected
  output$cm_timeline_message <- renderUI({
    if (is.null(input$usubj_select) || input$usubj_select == "") {
      div(
        style = "padding:20px; font-size:16px; color:#555;
        justify-content: center;
        align-items: center;
        text-align: center;",
        "Please select a Subject (USUBJID) to view the CM timeline."
      )
    }
  })
  
  # Plot the timeline
  output$cm_timeline <- renderPlot({
    # Our variables for the subject
    si <- subject_info()
    cs <- cm_subject()

    # If not found show error
    if (nrow(si) == 0L) {
      return(ggplot2::ggplot() + ggplot2::labs(title = "No subject info found") +
               ggplot2::theme_minimal(base_size = 13))
    }
    
    # Get the treament start and end dates
    rfstd <- suppressWarnings(lubridate::ymd(si$RFSTDTC[1]))
    rfend <- suppressWarnings(lubridate::ymd(si$RFENDTC[1]))
    
    if (nrow(cs) == 0L) {
      return(ggplot2::ggplot() +
               ggplot2::labs(title = paste0("No CM recorded for ", si$USUBJID[1])) +
               ggplot2::theme_minimal(base_size = 13))
    }
    
    # Order CM rows for y
    levels_y <- cm_levels()
    cs$CMTRT_f <- factor(cs$CMTRT, levels = cm_levels())
    
    # Create equal month spacing but allow day plotting
    month_slot <- function(d) 12L * lubridate::year(d) + (lubridate::month(d) - 1L)
    
    # helper functiont that helps plots the date
    x_pos <- function(d) {
      d <- as.Date(d)
      mi <- month_slot(d)
      frac <- (lubridate::mday(d) - 1) / as.integer(lubridate::days_in_month(d))
      mi + frac
    }
    
    # helper function that helps make the date nicely formatted
    label_ym <- function(mi) {
      mi <- as.numeric(mi)
      slot <- floor(mi + 1e-9)     
      y  <- slot %/% 12L
      m  <- (slot %% 12L) + 1L
      sprintf("%04.0f-%02.0f", y, m)
    }
    
    
    # x positions for bars and trial lines
    # For the cs polts rows w no start date will stay NA and have no bar drawn
    cs$x_start <- ifelse(!is.na(cs$start_date), x_pos(cs$start_date), NA_real_)
    cs$x_end   <- ifelse(!is.na(cs$end_date),   x_pos(cs$end_vis),   NA_real_)
    rfstd_x <- if (!is.na(rfstd)) x_pos(rfstd) else NA_real_
    rfend_x <- if (!is.na(rfend)) x_pos(rfend) else NA_real_
    
    # Limits from rows that actually have x's (plus ref lines)
    finite_x <- is.finite(cs$x_start) | is.finite(cs$x_end)
    any_data <- any(finite_x) || is.finite(rfstd_x) || is.finite(rfend_x)
    
    # Limits pads 0.1 to make sure that plots are shown
    x_min <- min(cs$x_start[is.finite(cs$x_start)], na.rm = TRUE)
    x_max_base <- suppressWarnings(max(
      cs$x_end[is.finite(cs$x_end)],
      rfstd_x, rfend_x, na.rm = TRUE
    ))
    
    lims <- c(floor(x_min) - 0.5, ceiling(x_max_base) + 0.5)
    
    # Extend start-only bars to the right edge; leave no-start rows as NA
    plot_right <- lims[2] - 0.05
    cs$x_end_final <- dplyr::case_when(
      is.finite(cs$x_start) & !is.finite(cs$x_end) ~ plot_right,  # start-only → extend
      TRUE                                         ~ cs$x_end      # normal / no-start stays as is (NA)
    )
    
    # ensure a visible sliver for single-day
    cs$x_end_final <- ifelse(
      is.finite(cs$x_start) & (!is.finite(cs$x_end_final) | cs$x_end_final <= cs$x_start),
      cs$x_start + 0.02,
      cs$x_end_final
    )
  
    # Integer month ticks only (prevents duplicate labels)
    base_breaks <- seq(from = floor(lims[1]), to = ceiling(lims[2]), by = 1L)
    
    # Ggplot for the timeline
    g <- ggplot2::ggplot(cs) +
      ggplot2::geom_segment(
        ggplot2::aes(x = x_start, xend = x_end_final, y = CMTRT_f, yend = CMTRT_f),
        linewidth = 4, lineend = "round", colour = "#2c3e50", na.rm = TRUE
      ) +
      # Creates the start/end treatment reference lines
      (if (!is.na(rfstd_x))
        ggplot2::geom_vline(xintercept = rfstd_x, linetype = "dashed",
                            colour = "#d62728", linewidth = 0.9) else NULL) +
      (if (!is.na(rfend_x))
        ggplot2::geom_vline(xintercept = rfend_x, linetype = "dotted",
                            colour = "#1f77b4", linewidth = 0.9) else NULL) +
      
      # sets up the x axis for the timeline dates
      ggplot2::scale_x_continuous(
        limits = lims,
        breaks = base_breaks,
        labels = label_ym,
        expand = ggplot2::expansion(add = c(0.05, 0.05))
      ) +
      # This removes undated CM labels
      ggplot2::scale_y_discrete(drop = TRUE, na.translate = FALSE) + 
      # Makes sure dates dont overlap
      ggplot2::guides(x = ggplot2::guide_axis(check.overlap = TRUE)) +
      # title label for the timeline
      ggplot2::labs(
        title = paste0("CM Timeline — Subject: ", si$USUBJID[1]),
        subtitle = paste0(
          "ARM: ", si$ARM[1],
          if (!is.na(rfstd)) paste0("\nTreatment Start (dashed red): ", format(rfstd, "%Y-%m-%d")) else "",
          if (!is.na(rfend)) paste0("\nTreatment End (dotted blue): ", format(rfend, "%Y-%m-%d")) else ""
        ),
        x = NULL, y = NULL
      ) +
      # creates the theme
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0, face = "bold", colour = "black"),
        plot.subtitle = ggplot2::element_text(hjust = 0, colour = "black"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(colour = "black"),
        axis.text.x = ggplot2::element_text(colour = "black", angle = 30, hjust = 1),
        axis.ticks.x = ggplot2::element_line()
      )
    g
  })
  
  # ============ Patient Timeline BreakDown Table ================
  # data for the table
  cm_table_data <- reactive({
    cs <- cm_subject()
    if (nrow(cs) == 0L) return(cs[0, ])
  
  # So changed it so if there is start dates itll only show unique start and end dates
  # If theres no start date then itll show up once in the table
  # Also added sort flag so itll be pushed to the bottom with no dates
  # Flag rows with valid dates
  cs <- cs %>%
    dplyr::mutate(
      has_start = !is.na(start_date),
      has_end = !is.na(end_date)
      )
  
  # distinct intervals for rows that DO have both dates
  both_dates <- cs %>%
    dplyr::filter(has_start & has_end) %>%
    dplyr::distinct(CMTRT, start_date, end_date, .keep_all = FALSE) %>%
    dplyr::mutate(
      CM = as.character(CMTRT),
      # For sorting
      start_ord = as.Date(start_date),
      # Displayed dates (you can chagne the format here)
      `Start Date` = format(as.Date(start_date), "%Y-%m-%d"),
      `End Date` = format(as.Date(end_date), "%Y-%m-%d"),
      sort_flag = 0L
    ) %>%
    dplyr::select(CM, `Start Date`, `End Date`, start_ord, sort_flag)
  start_only <- cs %>%
    dplyr::filter(has_start & !has_end) %>%
    dplyr::distinct(CMTRT, start_date, end_date, .keep_all = FALSE) %>%
    dplyr::mutate(
      CM = as.character(CMTRT),
      # For sorting
      start_ord = as.Date(start_date),
      `Start Date` = format(as.Date(start_date), "%Y-%m-%d"),
      `End Date` = "Ongoing",
      sort_flag = 0L
    ) %>%
    dplyr::select(CM, `Start Date`, `End Date`, start_ord, sort_flag)
  # C) for CMs with NO valid-dated (no start date) rows at all, include exactly one row with blanks
  no_dates <- cs %>%
    dplyr::filter(!has_start & !has_end) %>%
    dplyr::distinct(CMTRT) %>%
    dplyr::transmute(
      CM         = as.character(CMTRT),
      start_ord  = as.Date("0001-01-01"),
      `Start Date` = "",
      `End Date`   = "",
      sort_flag  = 1L
    )

  # Combine and order by dates
  dplyr::bind_rows(both_dates, start_only, no_dates) %>%
    dplyr::select(CM, `Start Date`, `End Date`)
  })

  
  # Output the table
  output$cm_timeline_table <- renderUI({
    df <- cm_table_data()
    # Edge case no data
    if (nrow(df) == 0L) {
      return(tags$div(style = "padding:10px;", "No ConMed recorded for this subject."))
    }
    
    # So add that same levels from the timeline to the table
    lv <- cm_levels() 
    # Add a guard if there is not start dates at all
    
    if (length(lv) == 0L) {
      # (edge case)
      df <- df %>% dplyr::arrange(CM, `Start Date`, `End Date`)
    } else {
      # This will sort the data to match CM but reverse to account for mirroring
      df <- df %>%
        dplyr::mutate(.ord = match(CM, rev(lv)), .ord = dplyr::if_else(is.na(.ord), Inf, .ord)) %>%
        dplyr::arrange(.ord, `Start Date`) %>%
        dplyr::select(-.ord)
    }
    # header
    head_row <- tags$tr(
      lapply(names(df), tags$th
    ))
    
    # body
    body_rows <- lapply(seq_len(nrow(df)), function(i) {
      tags$tr(
        tags$td(df$CM[i]),
        tags$td(df$`Start Date`[i]),
        tags$td(df$`End Date`[i])
      )
    })
    
    tags$table(
      tags$thead(head_row),
      tags$tbody(body_rows)
    )
  })
}