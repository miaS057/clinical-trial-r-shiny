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
    
    # data parsing
    df$CMSTDTC_parsed <- suppressWarnings(lubridate::ymd(df$CMSTDTC))
    df$CMENDTC_parsed <- suppressWarnings(lubridate::ymd(df$CMENDTC))
    
    # visible start/end
    df$start_date <- as.Date(df$CMSTDTC_parsed)
    df$end_date <- as.Date(dplyr::coalesce(df$CMENDTC_parsed, df$CMSTDTC_parsed))
    
    # fix clear data errors and make single-day visible
    df$end_date <- ifelse(is.na(df$end_date) | df$end_date < df$start_date,
                          df$start_date, df$end_date)
    df$end_date <- as.Date(df$end_date)
    
    df$end_vis <- pmax(df$end_date, df$start_date + lubridate::days(1))
    
    df
  })
  
  # Plot the timeline
  output$cm_timeline <- renderPlot({
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
    levels_y <- cs |>
      dplyr::arrange(start_date, end_vis, CMTRT) |>
      dplyr::pull(CMTRT) |>
      unique() |>
      rev()
    cs$CMTRT_f <- factor(cs$CMTRT, levels = levels_y)
    
    # Limits with small padding
    x_min <- min(cs$start_date, na.rm = TRUE)
    x_max <- max(cs$end_vis, rfstd, rfend, na.rm = TRUE)
    pad <- lubridate::days(7)
    lims <- c(x_min - pad, x_max + pad)
    
    # target tick count from rendered width (fallback if NULL)
    w <- session$clientData$output_cm_timeline_width
    if (is.null(w)) w <- 800
    target <- max(2, min(10, floor(w / 120))) 
    
    span_days <- as.numeric(diff(lims), units = "days")
    # choose step given span and target relative to the timeline
    step <- dplyr::case_when(
      span_days <= 60 ~ "1 week",
      span_days <= 150 & target >= 6 ~ "2 weeks",
      span_days <= 240 ~ "1 month",
      span_days <= 480 ~ "2 months",
      span_days <= 900 ~ "3 months",
      span_days <= 1500 ~ "6 months",
      TRUE ~ "1 year"
    )
    
    # helper: build sequence at chosen step
    seq_by <- function(from, to, by) {
      # snap to nice boundaries
      unit <- dplyr::case_when(
        grepl("week", by)   ~ "week",
        grepl("month", by)  ~ "month",
        grepl("year", by)   ~ "year",
        TRUE ~ "day"
      )
      from2 <- switch(unit,
                      week = lubridate::floor_date(from, "week"),
                      month = lubridate::floor_date(from, "month"),
                      year = lubridate::floor_date(from, "year"),
                      lubridate::floor_date(from, "day")
      )
      seq.Date(from2, lubridate::ceiling_date(to, unit), by = by)
    }
    
    # This will ensure there will be two dates at all times
    eps <- lubridate::days(1)
    breaks_adaptive <- function(l) {
      inside <- seq_by(l[1], l[2], step)
      if (length(inside) > target) {
        inside <- inside[round(seq(1, length(inside), length.out = target))]
      }
      # endpoints
      e1 <- l[1] + eps
      e2 <- l[2] - eps
      
      # drop any inside tick whose *label text* equals an endpoint label
      lab <- function(x) format(x, "%Y-%m")
      inside <- inside[!(lab(inside) %in% c(lab(e1), lab(e2)))]
      
      sort(unique(c(e1, inside, e2)))
    }
    
    g <- ggplot2::ggplot(cs) +
      ggplot2::geom_segment(
        ggplot2::aes(x = start_date, xend = end_vis, y = CMTRT_f, yend = CMTRT_f),
        linewidth = 4, lineend = "round", colour = "#2c3e50"
      ) +
      # Trial start line
      (if (!is.na(rfstd))
        ggplot2::geom_vline(
          xintercept = rfstd,  
          linetype = "dashed", 
          colour = "#d62728", 
          linewidth = 0.9
          )
       else NULL) +
      # Trial end line
      (if (!is.na(rfend))
        ggplot2::geom_vline(
          xintercept = rfend,  
          linetype = "dotted", 
          colour = "#1f77b4", 
          linewidth = 0.9
          )
       else NULL) +
      ggplot2::scale_x_date(
        limits = lims,
        breaks = breaks_adaptive,
        labels  = scales::label_date("%Y-%m"),
        guide = ggplot2::guide_axis(check.overlap = TRUE),
        expand  = ggplot2::expansion(mult = c(0.02, 0.02))
      ) +
      ggplot2::labs(
        title = paste0("CM Timeline — Subject: ", si$USUBJID[1]),
        subtitle = paste0(
          "ARM: ", si$ARM[1],
          if (!is.na(rfstd)) 
            paste0("\nTreatment Start (dashed red): ", format(rfstd, "%Y-%m-%d")) 
          else "",
          if (!is.na(rfend)) 
            paste0("\nTreatment End (dotted blue): ", format(rfend, "%Y-%m-%d")) 
          else ""
        ),
        x = NULL, y = NULL
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0, face = "bold", colour = "black"),
        plot.subtitle = ggplot2::element_text(hjust = 0, colour = "black"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(colour = "black"),
        axis.text.x = ggplot2::element_text(colour = "black", angle = 30, hjust = 1)
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
    dplyr::mutate(has_dates = !is.na(start_date) & !is.na(end_date))
  
  # distinct intervals for rows that DO have both dates
  with_dates <- cs %>%
    dplyr::filter(has_dates) %>%
    dplyr::distinct(CMTRT, start_date, end_date, .keep_all = FALSE) %>%
    dplyr::mutate(
      CM = as.character(CMTRT),
      # For sorting
      start_ord = as.Date(start_date),
      # Displayed dates (you can chagne the format here)
      `Start Date` = format(as.Date(start_date), "%Y-%m-%d"),
      `End Date` = format(as.Date(end_date),   "%Y-%m-%d"),
      sort_flag = 0L
    ) %>%
    dplyr::select(CM, `Start Date`, `End Date`, start_ord, sort_flag)
  
  # B) for CMs with NO valid-dated rows at all, include exactly one row with blanks
  no_date_once <- cs %>%
    dplyr::group_by(CMTRT) %>%
    dplyr::summarise(any_dates = any(has_dates), .groups = "drop") %>%
    dplyr::filter(!any_dates) %>%                 # only CMs with zero valid-dated rows
    dplyr::transmute(
      CM = as.character(CMTRT),
      # For sorting
      start_ord = as.Date("0001-01-01"),
      `Start Date` = "",
      `End Date` = "",
      sort_flag = 1L
    )
  
  # Combine and order by dates
  dplyr::bind_rows(with_dates, no_date_once) %>%
    dplyr::arrange(sort_flag, dplyr::desc(start_ord), CM) %>% 
    dplyr::select(CM, `Start Date`, `End Date`)
  })

  
  # same HTML style as the donut chart
  output$cm_timeline_table <- renderUI({
    df <- cm_table_data()
    # Edge case no data
    if (nrow(df) == 0L) {
      return(tags$div(style = "padding:10px;", "No ConMed recorded for this subject."))
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