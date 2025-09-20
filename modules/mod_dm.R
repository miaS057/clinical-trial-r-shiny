# Load libraries
library(dplyr)
library(plotly)
library(tidyr)
library(stringr)
library(bslib)
library(shiny)
library(lubridate)
library(data.table)
library(treemapify)
library(gt)
library(pharmaversesdtm)

data(package = "pharmaversesdtm")

# --- Import data.R --- #
#source("data.R")

# --- Load in dm data --- #
#dm <- get_dm()

# ---------------------------------------------------------------------------- #
# ------------------------------------ UI ------------------------------------ #
# ---------------------------------------------------------------------------- #


mod_dm_ui <- function() {
  dm_plot_cards
}

# Get min and max date for select_date
min_date <- as.Date(min(dm$RFSTDTC, na.rm=TRUE))
max_date <- as.Date(max(dm$RFSTDTC, na.rm=TRUE))

select_site <- radioButtons(
  inputId = "site_select",
  label = "Selected site to display",
  choices = c("All sites combined", unique(dm$SITEID)),
  selected = "All sites combined",
  inline = TRUE)

select_date <- sliderInput(
  inputId = "date_select",
  label = "Selected date range based on trial start date",
  min = min_date,
  max = max_date,
  value = c(min_date, max_date),
  timeFormat = "%Y-%m-%d")

# Card for filter selections
filters <- card(
  full_screen = TRUE,
  card_body(
    min_height = 120,
    layout_column_wrap(
      width = 1/2,
      card(full_screen = TRUE,select_site),
      card_body(full_screen = TRUE, padding = c(0, 0, 0, 25), select_date)
    )
  )
)

# Card for demographics summary table
dm_summary_table <- card(
  full_screen = TRUE,
  card_body(
    min_height = 200,
    padding = c(50, 0, 30, 0),
    gt_output("summary_table")
  )
)

dm_plot_cards <- card(
  full_screen = TRUE,
  filters,
  card_body(
    padding = c(20, 0, 0, 0),
    layout_column_wrap(
      width = 1/2,
      plotlyOutput(outputId = "age_plot"),
      plotlyOutput(outputId = "race_plot"),
      card_body(
        padding = c(40, 0, 0, 0),
        plotlyOutput(outputId = "sex_plot")
      ),
      card_body(
        padding = c(40, 0, 0, 0),
        plotOutput(outputId = "country_plot")
      )
    ),
    dm_summary_table
  )
)




# ---------------------------------------------------------------------------- #
# ----------------------------- HELPER FUNCTIONS ----------------------------- #
# ---------------------------------------------------------------------------- #

# Creates a dataframe formatted for gt table with each treatment arm as a column
dm_table_form <- function(df, category){
  df <- df %>% 
    group_by(ARM)
  
  if(category == "RACE") {df <- count(df, RACE)}
  else {df <- count(df, SEX)}
  
  # Calculate percentages
  df <- df %>% 
    mutate(Total = paste0(n, " (", scales::percent(prop.table(n)), ")")) 
  df$n <- NULL
  
  # Spread ARM to be in the format for gt table
  df <- df %>% spread(ARM, Total) %>% 
    # Reformat row names 
    cbind(Category = paste0(category, ", n (%)"))
  
  colnames(df)[which(names(df) == category)] <- "Variables"
  
  return (df)
}


# Creates a dataframe formatted for gt table with statistics of the 
# overall dm dataframe 
dm_total_table_form <- function(df, category){
  # Calculate percentages
  df <- df %>% 
    mutate(Total = paste0(n, " (", scales::percent(prop.table(n)), ")")) 
  colnames(df)[which(names(df) == category)] <- "Variables"
  df$n <- NULL
  
  return (df)
}


dm_age_table_form <- function(df, category = ""){
  if (category == "") {
    df <- df %>% group_by(ARM)
  }
  
  # Calculate statistics
  df <- df %>% 
    summarise(
      mean_sd = paste0(round(mean(AGE, na.rm = TRUE), 1), " (", 
                       round(sd(AGE, na.rm = TRUE), 1), ")"),
      median = paste0("", round(median(AGE), 1)),
      range = paste0(min(AGE), ", ", max(AGE))
    )
  colnames(df)[which(names(df) == "mean_sd")] <- "Mean (SD)"
  colnames(df)[which(names(df) == "median")] <- "Median"
  colnames(df)[which(names(df) == "range")] <- "Range"
  
  # Reformat dataframe for gt table
  if (category == "total"){
    df$ARM <- c("Total")
    df <- df[,c(4,1,2,3)]
    
    df <- df %>%
      pivot_longer(cols = -1, names_to = "Variables", values_to = "Total")
    df$ARM <- NULL
  }
  
  else{
    df <- df %>%
      pivot_longer(cols = -1, names_to = "Variables", values_to = "val")
    
    df <- df %>%
      spread(ARM, val) %>%
      cbind(Category = "Age")
  }
  
  return (df)
}




# ---------------------------------------------------------------------------- #
# ---------------------------------- SERVER ---------------------------------- #
# ---------------------------------------------------------------------------- #
mod_dm_server <- function(input, output, dm_r) {
  
  # Only get rows with RFSTDTC within the range specified by select_date
  dm <- reactive({
    dm_r()[as.Date(dm_r()$RFSTDTC) %inrange%
             as.Date(input$date_select),,drop=FALSE]
  })
  
  
  # ------------ Age Boxplot ------------ #
  output$age_plot <- renderPlotly({
    
    if(input$site_select == "All sites combined"){
      age_df <- dm()
    }
    if(input$site_select != "All sites combined"){
      age_df <- dm() %>% 
        filter(SITEID == input$site_select)
    }
    
    # If there is no data based on filters, return an empty plot
    if(nrow(age_df) == 0){
      empty_plot <- ggplot() +
        labs(
          title = "Age boxplot by treatment and sex",
          x = "Treatment",
          y = "Age")
      return(empty_plot)
    }
    
    age_by_arm_plot <- ggplot(age_df, aes(
      x = ARM,
      y = AGE,
      fill = SEX)) +
      geom_boxplot() +
      labs(
        title = "Age by treatment and sex",
        x = "Treatment",
        y = "Age",
        fill = "Sex") +
      theme(
        legend.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45)
      )
    return(ggplotly(age_by_arm_plot, tooltip = "text") %>%
             layout(boxmode = "group"))
  })
  
  
  # ------------ Race Bar Chart ------------ #
  output$race_plot <- renderPlotly({
    
    # Wrap values in RACE columns  
    wrap_label <- dm() %>% 
      mutate(RACE = str_wrap(RACE, width = 17))
    
    # Set a color for each race so the color for each race doesn't change if a 
    # race value is filtered out of the dataframe
    palette1_named <- setNames(
      object = scales::hue_pal()(length(unique(wrap_label$RACE))),
      nm = unique(wrap_label$RACE))
    
    if(input$site_select == "All sites combined"){
      race_df <- dm() %>% 
        group_by(ARM, RACE) %>% count(RACE)
    }
    else{
      race_df <- dm() %>% 
        filter(SITEID == input$site_select) %>%
        group_by(ARM, RACE) %>% 
        count(RACE)
    }
    
    # Make values in RACE columns also in wrapped form
    race_df <- race_df %>% 
      mutate(RACE = str_wrap(RACE, width = 17))
    
    race_by_arm_plot <- ggplot(race_df, aes(
      x = ARM,
      y = n,
      text = paste0("Count: ", n),
      fill = RACE)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Number of subjects in each treatment by race",
        x = "Treatment",
        y = "Count",
        fill = "Race") +
      scale_fill_manual(
        values = palette1_named) +
      theme(
        legend.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45)
      )
    return(ggplotly(race_by_arm_plot, tooltip = "text"))
    
  })
  
  
  
  # ------------ Sex Stacked Bar Chart ------------ #
  
  output$sex_plot <- renderPlotly({
    
    if(input$site_select == "All sites combined"){
      sex_df <- dm() %>% 
        count(SEX, ARM)
    }
    if(input$site_select != "All sites combined"){
      sex_df <- dm() %>% 
        filter(SITEID == input$site_select) %>%
        count(SEX, ARM)
    }
    
    sex_by_arm_plot <- ggplot(sex_df, aes(
      x = ARM,
      y = n,
      text = paste0("Count: ", n),
      fill = SEX)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Number of subjects in each treatment by sex",
        x = "Treatment",
        y = "Total Number of Subjects",
        fill = "Sex") +
      theme(
        legend.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45)
      )
    return(ggplotly(sex_by_arm_plot, tooltip = "text"))
    
  })
  
  
  # ------------ Country and SiteID Treemap ------------ #
  output$country_plot <- renderPlot({
    
    if(input$site_select == "All sites combined"){
      country_df <- dm() %>% 
        group_by(COUNTRY, SITEID) %>% 
        count(COUNTRY)
    }
    if(input$site_select != "All sites combined"){
      country_df <- dm() %>% 
        filter(SITEID == input$site_select) %>%
        group_by(COUNTRY, SITEID) %>% 
        count(COUNTRY)
    }
    
    ggplot(country_df, aes(
      area = n,
      fill = COUNTRY,
      label = paste0("SiteID: ", SITEID, "\nNumber of Subjects: ", n),
      subgroup = COUNTRY)) +
      geom_treemap() +
      geom_treemap_text(
        place = "centre", 
        size = 12) +
      labs(
        title = "Number of subjects per site for each country") +
      theme(plot.title = element_text(size = 19))
    
  })
  
  
  
  # ------------ Summary Table ------------ #
  
  output$summary_table <- render_gt({
    
    # Create dataframes with total statistics for race, sex, and age
    race_total_df <- dm_total_table_form(count(dm(), RACE), "RACE")
    sex_total_df <- dm_total_table_form(count(dm(), SEX), "SEX")
    age_total_df <- dm_age_table_form(dm(), category = "total")
    
    # Create dataframes with statistics for race, sex, and age by treatment arms
    race_df <- dm_table_form(dm(), "RACE")
    race_df[is.na(race_df)] = "0 (0%)"
    sex_df <- dm_table_form(dm(), "SEX")
    age_df <- dm_age_table_form(dm())
    
    # Combine dataframes together
    combined_total_df <- rbind(race_total_df, sex_total_df)
    combined_total_df <- rbind(combined_total_df, age_total_df)
    
    combined_group_df <- rbind(race_df, sex_df)
    combined_group_df<- rbind(combined_group_df, age_df)
    
    combined_df <- merge(combined_group_df, combined_total_df, by = "Variables")
    
    # Calculate how many subjects are in each arm
    arm_total <- dm() %>% 
      count(ARM) 
    
    # Calculate how many subjects there are in total
    total <- data.frame("Total", nrow(dm()))
    names(total) <- c("ARM", "n")
    
    # Combine dataframes together
    arm_total <- rbind(arm_total, total)
    
    # Find names of treatment arms along with the total column
    col_names <- names(combined_df[, !names(combined_df) %in% 
                                     c("Variables", "Category")] )
    
    # Make the positions of the arm columns the same as in the combined_df
    arm_total <- arm_total[match(col_names, arm_total$ARM),]
    
    # Attach N = # to arms and total columns
    for (name in col_names){
      colnames(combined_df)[which(names(combined_df) == name)] <- paste0(
        name, " (N = ", arm_total[arm_total$ARM == name, "n"], ")")
    }
    
    # Make summary table
    gt_tbl <-
      combined_df |>
      group_by(Category) |>
      gt(rowname_col = "Variables") |>
      tab_stubhead(label = "Variables") |>
      tab_header(
        title = "Demographics Summary Table"
      ) |>
      cols_align(
        align = "center",
        columns = (!contains("Variables"))
      ) |>
      cols_hide(
        columns = Category
      )|>
      opt_vertical_padding(scale = 1) |>
      opt_horizontal_padding(scale = 3)
    
  })
  
}


