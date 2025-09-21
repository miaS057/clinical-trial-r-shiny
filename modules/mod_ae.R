# Load libraries
library(dplyr)
library(plotly)
library(tidyr)
library(stringr)
library(bslib)
library(shiny)
library(lubridate)
library(data.table)
library(gt)
library(reshape2)


# --- Import data.R --- #
source("data.R")

# --- Load dm data --- #
dm <- get_dm()

# --- Load ae data --- #
ae <- get_ae()

# ---------------------------------------------------------------------------- #
# ------------------------------------ UI ------------------------------------ #
# ---------------------------------------------------------------------------- #

mod_ae_ui <- function() {
  ae_plot_cards
}

# Join ae and dm 
# Filter out subjects that failed screening and have an 
# adverse effect relatedness of NA
join_ae_dm_df <- left_join(ae, dm, by = "USUBJID")
filtered_ae_dm_df <- join_ae_dm_df %>% 
  filter(ACTARM != "Screen Failure" &
          AEREL != "NA")

arm_select <- selectizeInput(
  inputId = "arm_select",
  label = "Selected treatment (Max 2 selections)",
  choices = unique(filtered_ae_dm_df$ACTARM),
  selected = unique(filtered_ae_dm_df$ACTARM)[1:2],
  multiple = TRUE,
  options = list(maxItems = 2, dropdownParent = 'body'))

sev_select <- selectizeInput(
  inputId = "sev_select",
  label = "Selected AE severity",
  choices = unique(ae$AESEV),
  selected = unique(filtered_ae_dm_df$AESEV),
  multiple = TRUE,
  options = list(dropdownParent = 'body'))

rel_select <- selectizeInput(
  inputId = "rel_select",
  label = "Selected relatedness",
  choices = unique(filtered_ae_dm_df$AEREL),
  selected = filtered_ae_dm_df$AEREL[filtered_ae_dm_df$AEREL != "NONE"],
  multiple = TRUE,
  options = list(dropdownParent = 'body'))

aedecod_select <- selectizeInput(
  inputId = "aedecod_select",
  label = "Selected AEDECOD",
  choices = sort(unique(filtered_ae_dm_df$AEDECOD)),
  selected = sort(unique(filtered_ae_dm_df$AEDECOD))[1:3],
  multiple = TRUE,
  options = list(dropdownParent = 'body'))

bodsys_select <- selectizeInput(
  inputId = "aebodsys_select",
  label = "Selected AEBODSYS",
  choices = sort(unique(filtered_ae_dm_df$AEBODSYS)),
  selected = sort(unique(filtered_ae_dm_df$AEBODSYS))[1:3],
  multiple = TRUE,
  options = list(dropdownParent = 'body'))

subj_select <- selectizeInput(
  inputId = "subj_select",
  label = "Selected USUBJID",
  choices = unique(filtered_ae_dm_df$USUBJID),
  selected = unique(filtered_ae_dm_df$USUBJID)[1],
  options = list(dropdownParent = 'body'))


# Card for filters
filters <- card(
  full_screen = TRUE,
  card(full_screen = TRUE, arm_select),
  card(full_screen = TRUE, sev_select),
  card(full_screen = TRUE, rel_select),
  card(full_screen = TRUE, aedecod_select),
  card(full_screen = TRUE, bodsys_select)
)


ae_plot_cards <- navset_card_tab(
  full_screen = TRUE,
  nav_panel(
    "General Data",
    card(
      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          filters
        ),
      card_body(
        padding = c(0, 0, 40, 0),
        plotlyOutput(outputId = "aedecod_butterfly_plot")
      )
    )),
    card_body(
      layout_column_wrap(
        width = 1/2,
        card_body(padding = c(0, 15, 50, 10), gt_output("decod_summary_table")),
        card_body(padding = c(0, 30, 50, 15), gt_output("bodsys_summary_table"))
      )
    )
  ),
  nav_panel(
    "Subject Data",
    card(
      full_screen = TRUE,
      card_body(
        padding = c(30, 0, 0, 0), 
        subj_select),
      layout_column_wrap(
        width = 1/2,
        card_body(
          plotOutput(outputId = "subj_ae_timeline_plot")
        ),
        card_body(
          padding = c(50, 0, 0, 0),
          gt_output("subj_summary_table"))
      )
        
    )
  )
)



# ---------------------------------------------------------------------------- #
# ----------------------------- HELPER FUNCTIONS ----------------------------- #
# ---------------------------------------------------------------------------- #

# Creates a dataframe formatted for gt table with treatment arms and 
# total as columns
ae_table_form <- function(df, category, total_count, arm_count){

  if(category == "AEDECOD") {
    tot_df <- df %>% count(AEDECOD)
    df <- df %>% group_by(ACTARM, AEDECOD) %>% count(AEDECOD)
  }
  else if(category == "AEBODSYS") {
    tot_df <- df %>% count(AEBODSYS)
    df <- df %>% group_by(ACTARM, AEBODSYS) %>% count(AEBODSYS)
  }
  merged_df <- merge(df, arm_count, by = "ACTARM")
  
  # Calculate percentages
  merged_df <- merged_df %>% 
    mutate(Npct = paste0(
      n.x, " (", round(((n.x / n.y) * 100), digits = 2), "%)"))
  merged_df$n.x <- NULL
  merged_df$n.y <- NULL
  merged_df <- merged_df %>% spread(ACTARM, Npct)
  
  # Make NA values into 0 (0%)
  merged_df[is.na(merged_df)] = "0 (0%)"
  
  # Create a total column 
  tot_df$Tot <- total_count
  tot_df <- tot_df %>% 
    mutate(Total = paste0(
      n, " (", round(((n / Tot) * 100), digits = 2), "%)"))
  tot_df$n <- NULL
  tot_df$Tot <- NULL
  
  merged_all <- merge(merged_df, tot_df, by = category)
  
  return (merged_all)
}

# Attaches N = # to treatment arm and total columns
attach_n <- function(df, category, total_count, arm_count){
  
  # Creates a dataframe with total number of 
  # occurences for the specified category
  total <- data.frame("Total", total_count)
  names(total) <- c("ACTARM", "n")
  
  ae_arm_total <- rbind(arm_count, total)
  
  # Get treatment arm names
  col_names <- names(df[, !(names(df) == category)])
  ae_arm_total <- ae_arm_total[match(col_names, ae_arm_total$ACTARM),]
  
  # Attach N = # to each treatment arm column and total column
  for (name in col_names){
    colnames(df)[which(names(df) == name)] <- paste0(
      name, " (N = ", ae_arm_total[ae_arm_total$ACTARM == name, "n"], ")")
  }
  
  return (df)
}

# Changes date into type Date
to_date <- function(date){
  return (as.Date(date))
} 



# ---------------------------------------------------------------------------- #
# ---------------------------------- SERVER ---------------------------------- #
# ---------------------------------------------------------------------------- #
mod_ae_server <- function(input, output, dm_r, ae_r) {
  dm <- reactive({dm_r() %>% 
      filter(ACTARM != "Screen Failure")})
  ae <- reactive({ae_r() %>% 
      filter(AEREL != "NA")})
  
  ae_dm_df <- reactive({left_join(ae(), dm(), by = "USUBJID")})
  
  # Get only relevant adverse events based on filters
  filtered_ae_dm_df <- reactive({
    ae_dm_df() %>% 
      filter(ACTARM %in% input$arm_select &
               AEREL %in% input$rel_select & 
             AESEV %in% input$sev_select)
  })
  
  filtered_ae_dm <- reactive({
    filtered_ae_dm_df()[c("USUBJID", "ACTARM", "AEDECOD", "AESEV", 
                          "AEREL", "AEBODSYS")]
  })
  
  # ------------ AEDECOD Butterfly Plot ------------ #
  output$aedecod_butterfly_plot <- renderPlotly({
    
    arm_1 <- input$arm_select[1]
    arm_2 <- input$arm_select[2]
    
    butterfly_palette <- setNames(
      object = scales::hue_pal()(length(unique(filtered_ae_dm()$AESEV))),
      nm = unique(filtered_ae_dm()$AESEV))
    
    arms_df <- filtered_ae_dm() %>% 
      filter(AEDECOD %in% input$aedecod_select) %>% 
      group_by(AEDECOD, ACTARM, AESEV) %>% 
      count(AEDECOD) 
    
    # Format n values to create a pyramid
    arms_pyramid <- arms_df %>%
      mutate(
        n = case_when(
          ACTARM == arm_1 ~ -n,
          TRUE ~ n
        )
      )
    
    # Create the range for the x-axis
    max_range <- arms_pyramid %>% 
      group_by(AEDECOD, ACTARM) %>% 
      summarise(n = sum(abs(n)))
    
    n_range <- range(max_range$n)

    range_1 <- c(-n_range[1], n_range[2])
    range_2 <- c(n_range[1], -n_range[2])
    
    # Use pretty() to make the x ticks be rounded numbers
    pretty_vec_1 <- pretty(range_1)
    pretty_vec_2 <- pretty(range_2)
  
    # Make the x-axis ticks
    n_range_seq <- pretty(
      c(pretty_vec_1, pretty_vec_2),
      n = 10
    )
    
    aedecod_sev_plot <- ggplot(arms_pyramid, aes(
      x = n,
      y = AEDECOD,
      text = paste0("Count: ", abs(n)),
      fill = AESEV)) +
      geom_col(width = 0.3) +
      scale_x_continuous(
        breaks  = n_range_seq,
        labels = abs(n_range_seq)) +
      expand_limits(
        x = range(n_range_seq)) +
      geom_vline(
        xintercept = 0, 
        color = "white", 
        linewidth = 0.5) +
      scale_fill_manual(
        values = butterfly_palette
      ) +
      labs(
        title = paste0("Count of adverse effects by AEDECOD and severity for\n", 
                       arm_1, " (left) and ", arm_2, " (right)"),
        x = "Count"
      ) + 
      theme(
        plot.margin = unit(c(0.5, 0, 0, 0.5), "inches")) 

    return(ggplotly(aedecod_sev_plot, tooltip = "text"))
  })
  
  
  # ------------ AEDECOD Summary Table ------------ #
  output$decod_summary_table <- render_gt({
    
    # Get the total number of rows in ae_dm_df
    total_rows <- nrow(ae_dm_df())
    
    # Get number of occurrences in AEDECODs for each treatment arm
    arm_rows <- ae_dm_df() %>% 
      filter(ACTARM %in% input$arm_select) %>% 
      count(ACTARM)
    
    # Filter based on AEDECOD filter selections
    decod_filtered_df <- filtered_ae_dm() %>% 
      filter(AEDECOD %in% input$aedecod_select)
    
    # Create dataframes for AEDECOD based on filter selections
    aedecod_table_df <- ae_table_form(decod_filtered_df, 
                                      "AEDECOD", total_rows, arm_rows)
    final_aedecod_table_df <- attach_n(aedecod_table_df, 
                                       "AEDECOD", total_rows, arm_rows)
    
    # Find treatment arm names
    col_names <- names(
      final_aedecod_table_df[, !(names(final_aedecod_table_df) == "AEDECOD")]
      )
    
    # Find AEDECODs that are selected but were filtered out when making 
    # final_aedecod_table_df because the AEREL or AESEV did not match
    zero_val_decod <- ae_dm_df() %>% 
      filter(AEDECOD %in% input$aedecod_select & 
               !(AEDECOD %in% final_aedecod_table_df$AEDECOD))
    zero_val_decod <- unique(zero_val_decod$AEDECOD)
    
    # Add a 0 (0%) to AEDECODs 
    npct <- rep("0 (0%)", times = length(zero_val_decod)) 
    zero_val_decod_df <- data.frame(zero_val_decod, npct, npct, npct)
    names(zero_val_decod_df) <- c("AEDECOD", col_names)
    
    # Combined dataframes together
    final_aedecod_table_zero_vals_df <- rbind(final_aedecod_table_df, 
                                              zero_val_decod_df)
    
    
    aedecod_gt_tbl <-
      final_aedecod_table_zero_vals_df |>
      gt(rowname_col = "AEDECOD")
    
    aedecod_gt_tbl <-
      aedecod_gt_tbl |>
      tab_stubhead(label = "AEDECOD") |>
      tab_header(
        title = "Selected AEDECOD Summary Table"
      ) |>
      tab_stub_indent(
        rows = everything(),
        indent = 3
      ) |>
      cols_align(
        align = "center",
        columns = c(!contains("AEDECOD"))
      ) |> 
      cols_hide(
        columns = !contains(c(input$arm_select, "Total"))
      ) |>
      opt_vertical_padding(scale = 1)
    
  })
  
  
  # ------------ AEBODSYS Summary Table ------------ #
  
  output$bodsys_summary_table <- render_gt({
    
    # Get the total number of rows in ae_dm_df
    total_rows <- nrow(ae_dm_df())
    
    # Get number of occurrences in AEDECODs for each treatment arm
    arm_rows <- ae_dm_df() %>% 
      filter(ACTARM %in% input$arm_select) %>% 
      count(ACTARM)
    
    # Filter based on AEBODSYS filter selections
    bodsys_filtered_df <- filtered_ae_dm() %>% 
      filter(AEBODSYS %in% input$aebodsys_select)   
    
    # Create dataframes for AEBODSYS based on filter selections
    aebodsys_table_df <- ae_table_form(bodsys_filtered_df, 
                                       "AEBODSYS", total_rows, arm_rows)
    final_aebodsys_table_df <- attach_n(aebodsys_table_df, 
                                        "AEBODSYS", total_rows, arm_rows)
    
    # Find treatment arm names
    col_names <- names(
      final_aebodsys_table_df[, !(names(final_aebodsys_table_df) == "AEBODSYS")]
      )
    
    # Find AEBODSYS that are selected but were filtered out when making 
    # final_aebodsys_table_df because the AEREL or AESEV did not match
    zero_val_bodsys <- ae_dm_df() %>% 
      filter(AEBODSYS %in% input$aebodsys_select & 
               !(AEBODSYS %in% final_aebodsys_table_df$AEBODSYS))
    zero_val_bodsys <- unique(zero_val_bodsys$AEBODSYS)
    
    # Add a 0 (0%) to AEDECODs 
    npct <- rep("0 (0%)", times = length(zero_val_bodsys)) 
    zero_val_bodsys_df <- data.frame(zero_val_bodsys, npct, npct, npct)
    names(zero_val_bodsys_df) <- c("AEBODSYS", col_names)
    
    # Combined dataframes together
    final_aebodsys_table_zero_vals_df <- rbind(final_aebodsys_table_df, 
                                               zero_val_bodsys_df)
    
    
    bodsys_gt_tbl <-
      final_aebodsys_table_zero_vals_df |>
      gt(rowname_col = "AEBODSYS")
    
    bodsys_gt_tbl <-
      bodsys_gt_tbl |>
      tab_stubhead(label = "AEBODSYS") |>
      tab_header(
        title = "Selected AEBODSYS Summary Table"
      ) |>
      tab_stub_indent(
        rows = everything(),
        indent = 3
      ) |>
      cols_align(
        align = "center",
        columns = (!contains("AEBODSYS"))
      ) |>
      cols_hide(
        columns = !contains(c(input$arm_select, "Total"))
      ) |>
      opt_vertical_padding(scale = 1)|>
      opt_horizontal_padding(scale = 3)
    
    
  })
  
  
  # ------------ Subject AE Date Range Plot  ------------ #

  output$subj_ae_timeline_plot <- renderPlot({
    
    # Filter dataframe to only contain necessary information
    subj_df <- ae_dm_df() %>% 
      filter(USUBJID == input$subj_select)
    timeline_df <- subj_df[c("USUBJID", "ACTARM", "AEDECOD", "RFSTDTC", 
                             "RFENDTC", "AESTDTC", "AEENDTC")]
    
    # Filter out rows that have NA values for AESTDTC
    timeline_df <- timeline_df %>% 
      drop_na(AESTDTC)
    
    # Filter out rows that have proper date format
    timeline_df <- timeline_df %>% 
      filter(nchar(AESTDTC) == 10)
    
    # If timeline_df has no data, create a plot that only has a point for 
    # RFSTDTC and RFENDTC
    if(nrow(timeline_df) == 0){
      timeline_df <- subj_df
      timeline_df <- data.frame(timeline_df[c("AEDECOD")], 
                                lapply(timeline_df[c("RFSTDTC", 
                                                     "RFENDTC")], to_date) )
      melted_df <- melt(timeline_df, measure.vars = c("RFSTDTC", "RFENDTC"))
      
      min_date <- ymd(min(timeline_df$RFSTDTC)) - months(1)
      max_date <- ymd(min(timeline_df$RFENDTC)) + months(1)
      
      timeline_plot <- ggplot(melted_df, aes(
        x = value, 
        y = AEDECOD)) + 
        geom_point() +
        theme_minimal() +
        theme(
          aspect.ratio = 0.4, 
          axis.text = element_text(size = 7)) +
        scale_x_date(
          limits = c(min_date, max_date), 
          date_breaks = "1 month", 
          date_labels = "%b %d") +
        labs(
          x = "Date",
          y = "AEDECOD",
          title = "Date range for each adverse event"
        ) 
      return(timeline_plot)
    }
    
    # Get min and max date from the timeline_df
    min_date <- ymd(min(c(min(timeline_df$RFSTDTC), 
                          min(timeline_df$AESTDTC)))) - months(1)
    max_date <- ymd(max(c(max(timeline_df$RFENDTC), 
                          max(timeline_df$AEENDTC)))) + months(1)
    
    # Duplite AESTDTC and AEENDTC columns 
    timeline_df$AEENDTC_COPY <- timeline_df$AEENDTC
    timeline_df$AESTDTC_COPY <- timeline_df$AESTDTC
    
    # If AEENDTC is NA, replace it with value in AESTDTC
    timeline_df <- timeline_df %>%
      mutate(AEENDTC = ifelse(is.na(timeline_df$AEENDTC), AESTDTC, AEENDTC))
    
    # Change the type for the dates from character to Date
    timeline_df <- data.frame(timeline_df[c("AEDECOD", "AEENDTC_COPY")], 
                              lapply(timeline_df[c("RFSTDTC", "RFENDTC", 
                                                   "AESTDTC", "AEENDTC", 
                                                   "AESTDTC_COPY")], to_date) )
    
    # Make each AEDECOD be unique so multiple of the same AEDECODs with 
    # different time ranges can be plotted on seperate lines
    timeline_df$UNIQUEID <- seq_len(nrow(timeline_df))
    timeline_df<- timeline_df %>%
      mutate(AEDECOD_DATE = paste0(AEDECOD, "-", UNIQUEID))
    
    # Reformat timeline_df to be plotted with geom_line()
    melted_df <- melt(timeline_df, measure.vars = c("AESTDTC", "AEENDTC"))
    
  
    timeline_plot <- ggplot(melted_df, aes(
      x = value, 
      y = AEDECOD_DATE)) +
      geom_point(aes(
        x = AESTDTC_COPY,  
        color = "orange"), 
        size = 4) +
      geom_line(aes(
        color = "orange"),
      size = 5, 
      lineend = "round") +
      theme_minimal() +
      theme(
        aspect.ratio = 0.4, 
        axis.text = element_text(size = 7),
        axis.title.y = element_text(size = 15), 
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20),
        legend.position="none") +
      geom_point(aes(
        x = RFSTDTC)) +
      geom_point(aes(
        x = RFENDTC)) +
      scale_x_date(
        limits = c(min_date, max_date), 
        date_breaks = "1 month", 
        date_labels = "%b %d") +
      labs(
        x = "Date",
        y = "AEDECOD",
        title = "Date range for each adverse event"
      ) + 
      scale_y_discrete(
        labels = sub("-.*", "", melted_df$AEDECOD_DATE)) +
      theme(
        axis.text.x = element_text(angle = 45),
        axis.text = element_text(size = 12))
    
    return(timeline_plot)
    
  })
  
  
  # ------------ Subject AE Summary Table  ------------ #
  
  output$subj_summary_table <- render_gt({
    
    # Filter dataframe based on filter selection
    table_timeline_df <- ae_dm_df() %>% filter(USUBJID == input$subj_select)
    
    ae_usubjid <- table_timeline_df$USUBJID[1]
    ae_actarm <- table_timeline_df$ACTARM[1]
    ae_rfstdtc <- table_timeline_df$RFSTDTC[1]
    ae_rfendtc <- table_timeline_df$RFENDTC[1]
    
    # Get only necessary columns 
    table_timeline_df <- table_timeline_df[c("USUBJID", "ACTARM", "AEDECOD", 
                                             "AESTDTC", "AEENDTC")]
    
    subj_gt_tbl <-
      table_timeline_df |>
      gt(rowname_col = "AEDECOD")
    
    subj_gt_tbl <-
      subj_gt_tbl |>
      tab_stubhead(label = "AEDECOD") |>
      tab_header(
        title = "AEDECOD Summary Table",
        subtitle = paste0("Subject ", ae_usubjid, " in ", ae_actarm,
                          " treatment ARM ---", " RFSTDTC: ", 
                          ae_rfstdtc, " RFENDTC: ", ae_rfendtc)
      ) |>
      tab_stub_indent(
        rows = everything(),
        indent = 3
      ) |>
      cols_align(
        align = "center",
        columns = (!contains("AEDECOD"))
      ) |>
      cols_hide(
        columns = c("USUBJID", "ACTARM")
      ) |>
      opt_vertical_padding(scale = 1.5) |>
      opt_horizontal_padding(scale = 3)
  })
  
}








