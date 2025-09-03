# Load libraries
library(bslib)
library(shiny)
library(DT)

# import modules for each page
source("modules/mod_dm.R")
source("modules/mod_ds.R")
source("modules/mod_ae.R")
source("modules/mod_cm.R")


shinyUI(
  # display each domain in a different page
  # NEXT STEP: Make the UI look better using bslib
  navbarPage(
    # make sure to name your seperate page functions mod_dm_ui(id), etc...
    # if you want to test then you can comment out the pages you arent working on
    title = "Clinical Domains",
    tabPanel("DM", mod_dm_ui("dm")),
    tabPanel("DS", mod_ds_ui("ds")),
    tabPanel("AE", mod_ae_ui("ae")),
    tabPanel("CM", mod_cm_ui("cm"))
  )
)
