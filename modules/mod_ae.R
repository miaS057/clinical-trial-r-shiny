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

# ---------------------------------------------------------------------------- #
# ------------------------------------ UI ------------------------------------ #
# ---------------------------------------------------------------------------- #


mod_ae_ui <- function() {
  ae_plot_cards
}

dm_arm <- dm %>% 
  filter(ARM != "Screen Failure")
filtered_ae <- ae %>% filter(AEREL != "NA") %>% 
  sort_by(~ list(AEDECOD))

arm_select <- selectizeInput(
  inputId = "arm_select",
  label = "Selected treatment (Max 2 selections)",
  choices = unique(dm_arm$ARM),
  selected = unique(dm_arm$ARM)[1:2],
  multiple = TRUE,
  options = list(maxItems = 2))

sev_select <- selectInput(
  inputId = "sev_select",
  label = "Selected AE severity",
  choices = unique(ae$AESEV),
  selected = unique(filtered_ae$AESEV),
  multiple = TRUE,)

rel_select <- selectInput(
  inputId = "rel_select",
  label = "Selected relatedness",
  choices = unique(filtered_ae$AEREL),
  selected = filtered_ae$AEREL[filtered_ae$AEREL != "NONE"],
  multiple = TRUE,)

aedecod_select <- selectInput(
  inputId = "aedecod_select",
  label = "Selected AEDECOD",
  choices = unique(filtered_ae$AEDECOD),
  selected = unique(filtered_ae$AEDECOD)[1:5],
  multiple = TRUE,)



filters <- card(
  full_screen = TRUE,
  card_body(
    min_height = 150,
    layout_column_wrap(
      width = 1/2,
      card(full_screen = TRUE, arm_select),
      card(full_screen = TRUE, sev_select),
      card(full_screen = TRUE, rel_select),
      card(full_screen = TRUE, aedecod_select)
    )
  )
)


ae_plot_cards <- card(
  full_screen = TRUE,
  filters,
  card_body(
    padding = c(0, 0, 40, 0),
    plotlyOutput(outputId = "aedecod_butterfly_plot")
    )
  
)



# ---------------------------------------------------------------------------- #
# ---------------------------------- SERVER ---------------------------------- #
# ---------------------------------------------------------------------------- #
mod_ae_server <- function(input, output, dm_r, ae_r) {
  dm <- reactive({dm_r() %>% filter(ARM != "Screen Failure")})
  ae <- reactive({ae_r() %>% filter(AEREL != "NA")})
  
  ae_dm_df <- reactive({left_join(ae(), dm(), by = "USUBJID")})
  
  
  # ------------ AEDECOD Butterfly Plot ------------ #
  output$aedecod_butterfly_plot <- renderPlotly({
    
    arm_1 <- input$arm_select[1]
    arm_2 <- input$arm_select[2]
    
    arms_df <- ae_dm_df() %>% 
      filter(AEDECOD %in% input$aedecod_select & 
               AEREL %in% input$rel_select & 
               AESEV %in% input$sev_select &
               ARM %in% input$arm_select) %>% 
      group_by(AEDECOD, ARM, AESEV) %>% count(AEDECOD) 

    arms_pyramid <- arms_df %>%
      mutate(
        n = case_when(
          ARM == arm_1 ~ -n,
          TRUE ~ n
        )
      )

    max_range <- arms_pyramid %>% 
      group_by(AEDECOD, ARM) %>% 
      summarise(n = sum(abs(n)))
    
    n_range <- range(max_range$n)

    range_1 <- c(-n_range[1], n_range[2])
    range_2 <- c(n_range[1], -n_range[2])

    pretty_vec_1 <- pretty(range_1)
    pretty_vec_2 <- pretty(range_2)

    n_range_seq <- pretty(
      c(pretty_vec_1, pretty_vec_2),
      n = 10
    )
    aedecod_sev_plot <- ggplot(arms_pyramid,
                   aes(x = n,
                       y = AEDECOD,
                       text = paste0("Count: ", abs(n)),
                       fill = AESEV)) +
      geom_col() +
      scale_x_continuous(breaks  = n_range_seq,
                         labels = abs(n_range_seq)) +
      expand_limits(x = range(n_range_seq)) +
      geom_vline(xintercept = 0, color = "white", linewidth = 0.5) +
      labs(
        title = paste0("Count of adverse effects by AEDECOD and severity for\n", 
                       arm_1, " (left) and ", arm_2, " (right)"),
        x = "Count"
      ) + 
      theme(plot.margin = unit(c(0.5, 0, 0, 0.5), "inches"))

    return(ggplotly(aedecod_sev_plot, tooltip = "text"))
  })
  
  
  
  
  
  
  
  

  
  
}








