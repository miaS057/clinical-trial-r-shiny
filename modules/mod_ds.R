# Page for ds visuals
# pass in data needed from the server

mod_ds_ui <- function() {
  tagList(
    tags$style(HTML("
      .ds-grid {
        display: grid;
        grid-template-columns: 1fr;
        gap: 20px;
      }
      @media (min-width: 1100px) {
        .ds-grid {
          grid-template-columns: 1fr 1fr;
        }
      }
      .ds-card {
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 12px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
        padding: 14px;
      }
      .ds-card h3 {
        margin: 0 0 8px 0;
        font-weight: 600;
        font-size: 16px;
      }
      /* Plotly: remove grey backgrounds */
      .js-plotly-plot .plotly, .js-plotly-plot .plotly .svg-container {
        background: transparent !important;
      }
    ")),
    
    div(class = "ds-grid",
        div(class = "ds-card",
            h3("Subject Disposition Flow"),
            plotly::plotlyOutput("flow_sankey", height = "520px")
        ),
        div(class = "ds-card",
            h3("Disposition by ARM"),
            plotOutput("ds_table_plot", height = "520px")
        )
    ),
    br()
  )
}


mod_ds_server <- function(dm_r, ds_r, output) {
  dm <- reactive({
    req(dm_r())
    validate(need(is.data.frame(dm_r()), "DM must be a data.frame"))
    dm_r()
  })
  
  # All the unique USUBJIDS
  dm_arm <- reactive({
    dm() %>% 
    dplyr::distinct(USUBJID, ARM, ARMCD)
  })
  ds <- reactive({
    req(ds_r())
    validate(need(is.data.frame(ds_r()), "DS must be a data.frame"))
    ds_r()
  })
  
  # This is all the disposition events (added arm arm)
  dsdecod_ids <- reactive({
    ds() %>%
    dplyr::filter(DSCAT == "DISPOSITION EVENT") %>%
    dplyr::select(USUBJID, DSDECOD, DSTERM, DSSTDY, DSSEQ) %>% 
    dplyr::left_join(dm_arm(), by = "USUBJID") %>%
    group_by(DSDECOD)
  })
  
  # Every subjects final ds event (how far they got) 
  # NOTE: Use to calculate the percentages of completion
  # NOTE: Make sure to use this for the flow chart
  usub_final_ds <- reactive({
    dsdecod_ids() %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::slice_max(order_by = DSSEQ, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  })
  # How many of each event there were (sorted by ARM)
  diff_ds_events <- reactive({
    usub_final_ds() %>%
    dplyr::group_by(ARM, DSDECOD) %>%
    dplyr::summarise(
      n_subjects = n_distinct(USUBJID, na.rm = TRUE),
      .groups = "drop"
      )
  })
  # Use final events
  # This will make the buckets for the flow chart but will have lots of raw data
  flow_chart_buckets <- reactive({
    usub_final_ds() %>%
    dplyr::mutate(
      bucket = case_when(
        DSDECOD == "COMPLETED" ~ "Completed",
        (DSDECOD == "SCREEN FAILURE") | is.na(DSSTDY) ~ "Not Started",
        TRUE ~ "Stopped in Middle"
      )
    )
  })
  # This gets the counts at each bucket for the ARM and then the ds event
  flow_bucket_counts <- reactive({
    flow_chart_buckets() %>%
    dplyr::group_by(bucket, ARM, DSDECOD) %>%
    dplyr::summarize(
      subject_count = n_distinct(USUBJID),
      .groups = "drop"
    )
  })
  
  # ===================================================
  # =============== FLOW CHART BACK-END ===============
  # ===================================================
  # So this flow chart will be split by bucket Not Started -> Stopped in Middle -> Completed
  # Each bucket will be split by ARM (color coded)
  # You can hover over any part and it will tell you the breakdown of the reasons
  
  output$flow_sankey <- plotly::renderPlotly({
    fbc <- flow_bucket_counts()
    req(nrow(fbc) > 0)
    
    # These are the totals for each bucket by ARM
    totals_by_ab <- fbc %>%
      dplyr::group_by(ARM, bucket) %>%
      dplyr::summarise(
        value = sum(subject_count),
        .groups = "drop"
        
      )
    
    # These are the hover texts with the reasons
    hover_by_ab <- fbc %>%
      dplyr::arrange(ARM, bucket, dplyr::desc(subject_count)) %>%
      dplyr::group_by(ARM, bucket) %>%
      dplyr::summarise(
        hover = paste0(DSDECOD, ": ", subject_count, collapse = "<br>"),
        .groups = "drop"
      )
    
    # This will combine the total bucket counts and the reasons (hover text) together
    links_df <- totals_by_ab %>%
      dplyr::left_join(
        hover_by_ab,
        by = c("ARM", "bucket")
        )
    
    # This is our template for what the flow will look like
    arms    <- sort(unique(links_df$ARM))
    buckets <- c("Not Started", "Stopped in Middle", "Completed")
    nodes   <- c(arms, buckets)
    
    # 0-based indices for plotly (for the nodes)
    idx <- setNames(seq_along(nodes) - 1L, nodes)
    
    # makes the ARM and bucket names from human readable to plotly readable (Indexed)
    links_df <- links_df %>%
      dplyr::mutate(
        source = idx[ARM],
        target = idx[bucket]
      )
    
    # make the colors for the nodes (by ARM)
    arm_palette <- grDevices::rainbow(length(arms))
    arm_colors <- setNames(arm_palette, arms)
    links_df$link_color <- arm_colors[links_df$ARM]
    
    # Give a neutral grey color to the nodes on the left
    node_colors <- c(arm_colors, setNames(rep("#CCCCCC", length(buckets)), buckets))
    node_color_vec <- unname(node_colors[nodes])
    
    # The position of the node columns
    left_x <- rep(0.10, length(arms))
    left_y <- seq(0.15, 0.95, length.out = length(arms))
    right_x <- rep(0.85, length(buckets))
    right_y <- c(0.75, 0.50, 0.25)
    
    # Combine them for the left and right side
    node_x <- c(left_x, right_x)
    node_y <- c(left_y, right_y)
    
    # Label the nodes
    node_labels <- paste0("<b>", nodes, "</b>")
    
    # Build the flow chart
    p <- plotly::plot_ly(
      # The type of flow chart
      type = "sankey",
      arrangement = "fixed",
      
      # Create the nodes
      node = list(
        label = node_labels,
        color = unname(node_color_vec),
        pad = 16,
        thickness = 16,
        line = list(width = 0.5, color = "#666666"),
        x = node_x,
        y = node_y
      ),
      
      # Create the links from arm to node
      link = list(
        source = links_df$source,
        target = links_df$target,
        value = links_df$value,
        color = unname(links_df$link_color),
        # What the label says when hovered
        label = paste0(
          "<b>", links_df$ARM, " -> ", links_df$bucket, "</b><br><br>",
          "<b>Reasons</b><br>", links_df$hover
        ),
        hovertemplate = "%{label}<extra></extra>"
      ), 
      # This is the hover label font color and border color
      hoverlabel = list(
        bordercolor = "rgba(0,0,0,0.3)",
        font = list(color = "black", size = 12)
      )
    ) %>%
      # Create the layout (change margins as needed)
      plotly::layout(
        title = list(text = "", x = 0.5),
        font = list(size = 14),
        # give labels room so they don't clip
        margin = list(t = 40, l = 100, r = 120, b = 30),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      )
    p
  })
  # ===================================================
  # =============== DS TABLE BACK-END =================
  # ===================================================
  # This table just makes the DS event information easier to read
  output$ds_table_plot <- renderPlot({
    fbc <- flow_bucket_counts()
    req(nrow(fbc) > 0)
    
    # This will change the order of the buckets
    bucket_levels <- c("Not Started", "Stopped in Middle", "Completed")
    fbc <- fbc %>%
      dplyr::mutate(bucket = factor(bucket, levels = bucket_levels))
    
    # Get the ARM for Y axis
    arm_levels <- fbc %>% 
      dplyr::distinct(ARM) %>% 
      dplyr::arrange(ARM) %>% 
      dplyr::pull(ARM)
    
    # Force screen failure to the left side
    if ("Screen Failure" %in% arm_levels) {
      arm_levels <- c("Screen Failure", setdiff(arm_levels, "Screen Failure"))
    }
    
    # bucket totals per ARM
    bucket_totals <- fbc %>%
      dplyr::group_by(bucket, ARM) %>%
      dplyr::summarise(n = sum(subject_count), .groups = "drop") %>%
      dplyr::mutate(
        label = paste0("<b>", bucket, "</b>"), 
        is_header = TRUE
        )
  
    # Build the rows with the buckets and the indented reasons
    # Use a leading space for indentation. (ggplot keeps them.)
    reason_rows <- fbc %>%
      dplyr::group_by(bucket, DSDECOD, ARM) %>%
      dplyr::summarise(n = sum(subject_count), .groups = "drop") %>%
      dplyr::mutate(
        label = paste0("&nbsp;&nbsp;• ", DSDECOD, " &nbsp;&nbsp;•"),   # indentation
        is_header = FALSE
      ) %>%
      dplyr::select(bucket, ARM, label, n, is_header)
    
    # within each bucket: header first, then its reasons
    table_df <- dplyr::bind_rows(bucket_totals, reason_rows) %>%
      dplyr::arrange(bucket, dplyr::desc(is_header), label)
    
    # Build Y-axis
    levels_y <- unique(table_df$label)
    
    # ensure every label to ARM cell exists (fill missing combos with 0)
    table_df <- tidyr::complete(
      table_df,
      label = levels_y,
      ARM = arm_levels,
      fill = list(n = 0, is_header = FALSE)
    )
    
    # lock axes orders
    table_df <- table_df %>%
      dplyr::mutate(
        label = factor(label, levels = levels_y),
        ARM   = factor(ARM, levels = arm_levels)
      )
    
    
    #Create the table
    # draw: cells with counts; headers lightly shaded
    ggplot2::ggplot(table_df, ggplot2::aes(x = ARM, y = label)) +
      ggplot2::geom_tile(
        data = table_df %>% dplyr::filter(is_header),
        fill = "#F2F2F2", color = NA, width = 0.95, height = 0.95
      ) +
      # Blank ouy zeros
      ggplot2::geom_text(
        aes(label = ifelse(n == 0, "", n),
            fontface = ifelse(is_header, "bold", "plain")),
        size = 5
      ) +
      ggplot2::scale_y_discrete(limits = rev(levels_y)) +
      ggplot2::labs(title = "", x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid  = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
        axis.text.y = ggtext::element_markdown(
          size = 11
        ),
        plot.title  = ggplot2::element_text(hjust = 0, face = "bold")
      )
  })
}
  


