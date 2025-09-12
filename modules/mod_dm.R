# Load libraries
library(dplyr)
library(plotly)
<<<<<<< HEAD
library(tidyr)
=======
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
library(stringr)
library(bslib)
library(shiny)
library(lubridate)
library(data.table)
<<<<<<< HEAD
library(treemapify)
library(gt)
=======
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
library(pharmaversesdtm)

data(package = "pharmaversesdtm")

# ---------------------------------------------------------------------------- #
# ------------------------------------ UI ------------------------------------ #
# ---------------------------------------------------------------------------- #
<<<<<<< HEAD


mod_dm_ui <- function() {
  dm_plot_cards
=======
mod_dm_ui <- function() {
  dm_cards
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
}

min_date <- as.Date(min(dm$RFSTDTC, na.rm=TRUE))
max_date <- as.Date(max(dm$RFSTDTC, na.rm=TRUE))

<<<<<<< HEAD
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
=======
dm_cards <- page_fillable(
  layout_columns(
    card(radioButtons(
      inputId = "site_select",
      label = "Selected site to display",
      choices = c("All sites combined", unique(dm$SITEID)),
      selected = "All sites combined",
      inline = TRUE)
    ),
    card(sliderInput(
      inputId = "date_select",
      label = "Selected date range based on trial start date",
      min = min_date,
      max = max_date,
      value = c(min_date, max_date),
      timeFormat = "%Y-%m-%d"
    ))
  ),
  layout_columns(
    card(plotlyOutput(outputId = "age_plot")),
    card(plotlyOutput(outputId = "race_plot"))
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
  )
)


# ---------------------------------------------------------------------------- #
# ---------------------------------- SERVER ---------------------------------- #
# ---------------------------------------------------------------------------- #
mod_dm_server <- function(input, output, dm_r) {
  dm <- reactive({dm_r()})
<<<<<<< HEAD


  # ------------ Age Boxplot ------------ #
  output$age_plot <- renderPlotly({
    filtered_date_df <- dm()[as.Date(dm()$RFSTDTC) %inrange%
                             as.Date(input$date_select),,drop=FALSE]

=======
  # ------------ Age Boxplot ------------ #
  output$age_plot <- renderPlotly({
    filtered_date_df <- dm()[as.Date(dm()$RFSTDTC) %inrange% 
                             as.Date(input$date_select),,drop=FALSE]
    
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
    if(input$site_select == "All sites combined"){
      age_df <- filtered_date_df
    }
    if(input$site_select != "All sites combined"){
<<<<<<< HEAD
      age_df <- filtered_date_df %>% filter(SITEID == input$site_select)
    }

=======
      age_df <- filtered_date_df %>% filter(SITEID == input$site_select) 
    }
    
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
    if(nrow(age_df) == 0){
      empty_plot <- ggplot() +
        labs(title = "Age boxplot by treatment and sex",
             x = "Treatment",
             y = "Age")
      return(empty_plot)
    }
<<<<<<< HEAD

=======
    
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
    age_by_arm_plot <- ggplot(age_df, aes(
      x = ARM,
      y = AGE,
      fill = SEX)) +
      geom_boxplot() +
<<<<<<< HEAD
      labs(title = "Age by treatment and sex",
=======
      labs(title = "Age boxplot by treatment and sex",
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
           x = "Treatment",
           y = "Age",
           fill = "Sex") +
      theme(
        legend.text = element_text(size = 7),
        axis.text.x = element_text(angle = -45)
      )
    return(ggplotly(age_by_arm_plot, tooltip = "text") %>%
             layout(boxmode = "group"))
  })
<<<<<<< HEAD


=======
  
  
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
  # ------------ Race Bar Chart ------------ #
  output$race_plot <- renderPlotly({
    filtered_date_df <- dm()[as.Date(dm()$RFSTDTC) %inrange%
                             as.Date(input$date_select),,drop=FALSE]
<<<<<<< HEAD
    wrap_label <- dm() %>% mutate(RACE = str_wrap(RACE, width = 17))

    palette1_named <- setNames(
      object = scales::hue_pal()(length(unique(wrap_label$RACE))),
      nm = unique(wrap_label$RACE))

=======
    
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
    if(input$site_select == "All sites combined"){
      race_df <- filtered_date_df %>% group_by(ARM, RACE) %>% count(RACE)
    }
    else{
<<<<<<< HEAD
      race_df <- filtered_date_df %>% filter(SITEID == input$site_select) %>%
        group_by(ARM, RACE) %>% count(RACE)
    }
    race_df <- race_df %>% mutate(RACE = str_wrap(RACE, width = 17))

    race_by_arm_plot <- ggplot(race_df, aes(
      x = ARM,
      y = n,
      text = paste0("Count: ", n),
      fill = RACE)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Number of subjects in each treatment by race",
           x = "Treatment",
           y = "Count",
           fill = "Race") +
      scale_fill_manual(values = palette1_named) +
=======
      race_df <- filtered_date_df %>% filter(SITEID == input$site_select) %>% 
        group_by(ARM, RACE) %>% count(RACE)
    }
    race_by_arm_plot <- ggplot(race_df, aes(
      x = ARM, 
      y = n, 
      text = paste0("Count: ", n),
      fill = RACE)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Race barchart by treatment", 
           x = "Treatment", 
           y = "Count", 
           fill = "Race") +
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
      theme(
        legend.text = element_text(size = 7),
        axis.text.x = element_text(angle = -45)
      )
<<<<<<< HEAD

    return(ggplotly(race_by_arm_plot, tooltip = "text"))
  })



  # ------------ Sex Stacked Bar Chart ------------ #

  output$sex_plot <- renderPlotly({
    filtered_date_df <- dm()[as.Date(dm()$RFSTDTC) %inrange%
                               as.Date(input$date_select),,drop=FALSE]

    if(input$site_select == "All sites combined"){
      sex_df <- filtered_date_df %>% count(SEX, ARM)
    }
    if(input$site_select != "All sites combined"){
      sex_df <- filtered_date_df %>% filter(SITEID == input$site_select) %>%
        count(SEX, ARM)
    }

    sex_by_arm_plot <- ggplot(sex_df, aes(
      x=ARM,
      y=n,
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
        axis.text.x = element_text(angle = -45)
      )
    return(ggplotly(sex_by_arm_plot, tooltip = "text"))
  })


  # ------------ Country and SiteID Treemap ------------ #
  output$country_plot <- renderPlot({
    filtered_date_df <- dm()[as.Date(dm()$RFSTDTC) %inrange%
                               as.Date(input$date_select),,drop=FALSE]

    if(input$site_select == "All sites combined"){
      country_df <- dm() %>% group_by(COUNTRY, SITEID) %>% count(COUNTRY)
    }
    if(input$site_select != "All sites combined"){
      country_df <- filtered_date_df %>% filter(SITEID == input$site_select) %>%
        group_by(COUNTRY, SITEID) %>% count(COUNTRY)
    }
    #country_df <- dm() %>% group_by(COUNTRY, SITEID) %>% count(COUNTRY)

    ggplot(country_df, aes(
      area  =n,
      fill = COUNTRY,
      label = paste0("SiteID: ",SITEID,"\nNumber of Subjects: ",n),
      subgroup = COUNTRY)) +
      geom_treemap()+
      geom_treemap_text(place = "centre", size = 12) +
      labs(title = "Number of subjects per site for each country") +
      theme(plot.title = element_text(size = 20))

  })



  # ------------ Summary Table ------------ #

  output$summary_table <- render_gt({

    race_total_df <- dm() %>%
      filter(ARM != "Screen Failure") %>%
      count(RACE) %>%
      mutate(pct = paste0("(", scales::percent(prop.table(n)), ")"))
    race_total_df <- unite(race_total_df, Total, c(n, pct), sep=" ")
    colnames(race_total_df)[which(names(race_total_df) == "RACE")] <- "Variables"

    sex_total_df <- dm( ) %>%
      filter(ARM != "Screen Failure") %>%
      count(SEX) %>%
      mutate(pct = paste0("(", scales::percent(prop.table(n)), ")"))
    sex_total_df <- unite(sex_total_df, Total, c(n, pct), sep=" ")
    colnames(sex_total_df)[which(names(sex_total_df) == "SEX")] <- "Variables"


    age_total_df <- dm() %>%
      filter(ARM != "Screen Failure") %>%
      summarise(
        mean_sd = paste0(round(mean(AGE, na.rm = TRUE), 1), " (",
                         round(sd(AGE, na.rm = TRUE), 1), ")"),
        median = paste0("", round(median(AGE), 1)),
        range = paste0(min(AGE), ", ", max(AGE))
      )
    colnames(age_total_df)[which(names(age_total_df) == "mean_sd")] <- "Mean (SD)"
    colnames(age_total_df)[which(names(age_total_df) == "median")] <- "Median"
    colnames(age_total_df)[which(names(age_total_df) == "range")] <- "Range"

    age_total_df$ARM <- c("Total")
    age_total_df <- age_total_df[,c(4,1,2,3)]

    age_total_df <- age_total_df %>%
      pivot_longer(cols = -1, names_to = "Variables", values_to = "Total")
    age_total_df$ARM <- NULL

    combined_total_df <- rbind(race_total_df, sex_total_df)
    combined_total_df <- rbind(combined_total_df, age_total_df)




    sex_df <- dm() %>%
      group_by(ARM) %>% count(SEX) %>%
      filter(ARM != "Screen Failure") %>%
      mutate(pct = paste0("(", scales::percent(prop.table(n)), ")"))

    sex_df <- unite(sex_df, n_pct, c(n, pct), sep=" ") %>%
      spread(ARM, n_pct)


    race_df <- dm() %>%
      group_by(ARM) %>% count(RACE) %>%
      filter(ARM != "Screen Failure") %>%
      mutate(pct = paste0("(", scales::percent(prop.table(n)), ")"))

    race_df <- unite(race_df, n_pct, c(n, pct), sep=" ") %>%
      spread(ARM, n_pct)

    race_df[is.na(race_df)] = "0 (0%)"

    colnames(race_df)[which(names(race_df) == "RACE")] <- "Variables"
    colnames(sex_df)[which(names(sex_df) == "SEX")] <- "Variables"



    age_df <- dm() %>%
      group_by(ARM) %>%
      filter(ARM != "Screen Failure") %>%
      summarise(
        mean_sd = paste0(round(mean(AGE, na.rm = TRUE), 1), " (",
                         round(sd(AGE, na.rm = TRUE), 1), ")"),
        median = paste0("", round(median(AGE), 1)),
        range = paste0(min(AGE), ", ", max(AGE))
      )
    colnames(age_df)[which(names(age_df) == "mean_sd")] <- "Mean (SD)"
    colnames(age_df)[which(names(age_df) == "median")] <- "Median"
    colnames(age_df)[which(names(age_df) == "range")] <- "Range"

    age_df <- age_df %>%
      pivot_longer(cols = -1, names_to = "Variables", values_to = "val")

    age_df <- age_df %>%
      spread(ARM, val)


    combined_group_df <- rbind(race_df, sex_df)
    combined_group_df<- rbind(combined_group_df, age_df)

    combined_df <- merge(combined_group_df, combined_total_df, by = "Variables")



    gt_tbl <- gt(combined_df)
    gt_tbl <-
      combined_df |>
      gt(rowname_col = "Variables")

    gt_tbl |>
      tab_stubhead(label = "Variables") |>
      tab_header(
        title = "Demographics Summary Table"
      ) |>
      tab_row_group(
        label = "Race, n (%)",
        rows = everything()
      ) |>
      tab_row_group(
        label = "Sex, n (%)",
        rows = nchar(Variables) == 1
      ) |>
      tab_row_group(
        label = "Age",
        rows = (Variables == "Mean (SD)" | Variables == "Median" | Variables == "Range")
      ) |>
      tab_stub_indent(
        rows = everything(),
        indent = 3
      ) |>
      cols_align(
        align = "center",
        columns = (!contains("Variables"))
      )
  })

=======
    
    return(ggplotly(race_by_arm_plot, tooltip = "text"))
  })
  
>>>>>>> 52589dc798faeeea39d76e885843894348b9223d
}


