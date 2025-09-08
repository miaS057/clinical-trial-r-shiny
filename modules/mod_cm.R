# Page for cm visuals
# pass in data needed from the server

mod_cm_ui <- function() {
  tagList(
    div(
      uiOutput("arm_donuts")
    ),
    # CSS for donut chart makes it not wrap and keeps the donut charts side by side
    tags$style(HTML("
      .donut-grid {
      display: flex;
      flex-direction: row;
      flex-wrap: nowrap;
      gap: 60px;
      overflow-x: auto; 
      padding: 16px; 
    }
    
    .donut-card {
      flex: 0 0 500px;
      max-width: 500px;
      padding: 8px 8px 0 8px;
      border: 1px solid #e5e7eb;
      border-radius: 10px;
      background: #ffffff;
      box-shadow: 0 1px 3px rgba(0,0,0,0.05);
    }
    
    .donut-title {
      font-weight: 600;
      margin: 6px 8px 0 8px;
    }
    .dt-toggle {
      margin: 6px 0 10px;
      cursor: pointer;
      color: #2563eb;
      font-weight: 500;
    }
    .dt-toggle:hover { text-decoration: underline; }
    "))
  )
}


mod_cm_server <- function(dm_r, cm_r, output) {
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
  
  # This will show how many conmeds were taken of each type of conmed, split by arm
  cm_slices <- reactive({
    cm_by_arm() %>%
      dplyr::group_by(ARM, CMTRT) %>%
      dplyr::summarise(
        n_subjects = dplyr::n_distinct(USUBJID, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # This will calculate the total cm per arm
  total_cm_per_arm <- reactive({
    cm_by_arm() %>%
      dplyr::group_by(ARM) %>%
      dplyr::summarise(
        subjects_with_cm = dplyr::n_distinct(USUBJID, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Calculate a section "not taken" from total subjects - total cm per arm
  not_taken_slice <- reactive({
    arm_totals() %>%
      dplyr::left_join(total_cm_per_arm(), by = "ARM") %>%
      # This will calulate the new sums (replacing anything doesnt have cms with 0)
      dplyr::mutate(
        subjects_with_cm = tidyr::replace_na(subjects_with_cm, 0L),
        n_subjects = pmax(total_subjects - subjects_with_cm, 0L),
        CMTRT = "not taken"
      ) %>%
      dplyr::select(ARM, CMTRT, n_subjects)
  })
  
  # ===================================================
  # =============== DONUT CHART BACK-END ==============
  # ===================================================
  # This donut chart will show a donut chart for every arm that will have percentages of how many of each drug a certain ARM took
  # The merged donut data with all the necessary slices
  cm_donut_dat <- reactive({
    dplyr::bind_rows(
      cm_slices(),
      not_taken_slice()
    ) %>%
      dplyr::arrange(CMTRT)
  })
  
  # Now turn those slices into percentages
  cm_final_donut_dat <- reactive({
    cm_donut_dat() %>%
      dplyr::group_by(ARM) %>%
      dplyr::mutate(
        pct = n_subjects / sum(n_subjects),
        pct_label = scales::percent(pct)
      ) %>%
      dplyr::ungroup()
  })
  
  # Create the colors for each slice of the donut
  donut_palette <- reactive({
    trts <- cm_final_donut_dat() %>% dplyr::distinct(CMTRT) %>% dplyr::pull(CMTRT)
    base_cols <- grDevices::hcl.colors(max(3, length(trts)), "Set3")
    pal <- stats::setNames(base_cols[seq_along(trts)], trts)
    if ("not taken" %in% names(pal)) pal["not taken"] <- "#B0B0B0"
    pal
  })
  
  # ======== Helper Functions ========
  
  # This function will plot for one arm in a donut chart
  plot_donut_one_arm <- function(df_arm, pal) {
    ggplot2::ggplot(df_arm, ggplot2::aes(x = 2, y = n_subjects, fill = CMTRT)) +
      ggplot2::geom_col(width = 1, color = "white") +   # slices
      ggplot2::coord_polar(theta = "y") +
      ggplot2::scale_fill_manual(values = pal, drop = FALSE) +
      ggplot2::xlim(0.5, 2.5) +                         # creates the “hole”
      ggplot2::labs(title = unique(df_arm$ARM), fill = NULL) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      )
  }
  
  # This function will create the table for one arm, giving color keys and percentages
  table_data_one_arm <- function(df_arm, pal) {
    cols <- unname(pal[match(df_arm$CMTRT, names(pal))])
    
    df_arm %>%
      dplyr::arrange(dplyr::desc(pct)) %>%
      dplyr::mutate(
        Color = sprintf(
          '<span style="display:inline-block;width:12px;height:12px;background:%s;border:1px solid #888;border-radius:2px;"></span>',
          cols
        ),
        Percent = scales::percent(pct)
      ) %>%
      dplyr::select(Color, CMTRT, Percent) %>%
      dplyr::rename(Drug = CMTRT)
  }
  
  # Build the chart with the donut + table (1 each per ARM)
  output$arm_donuts <- renderUI({
    req(cm_final_donut_dat())
    arms <- unique(cm_final_donut_dat()$ARM)
    pal  <- donut_palette()
    
    # For each ARM, create plotOutput + datatableOutput
    ui_kids <- lapply(arms, function(arm) {
      arm_id <- gsub("\\W+", "_", arm)
      div(class = "donut-card",
          plotOutput(paste0("donut_", arm_id), height = 260),
          DT::dataTableOutput(paste0("table_", arm_id))
      )
    })
    
    # Register renders (scoped to this renderUI so they have the right data)
    lapply(arms, function(arm) {
      arm_id <- gsub("\\W+", "_", arm)
      df_arm <- isolate(cm_final_donut_dat()) %>% dplyr::filter(ARM == arm)
      
      output[[paste0("donut_", arm_id)]] <- renderPlot({
        plot_donut_one_arm(df_arm, pal)
      })
      
      output[[paste0("table_", arm_id)]] <- DT::renderDataTable({
        td <- table_data_one_arm(df_arm, pal)
        DT::datatable(
          td, 
          escape = FALSE, 
          rownames = FALSE,
          options = list(
            dom = "t", 
            paging = FALSE
            # ordering = TRUE
            ),
        )
      })
    })
    # Wrap them in the css so it stays side by side
    div(class = "donut-grid", do.call(tagList, ui_kids))
  })
  
}