# Load libraries
library(bslib)
library(shiny)
library(DT)
library(tidyverse)

# import modules for each page
source("modules/mod_dm.R")
source("modules/mod_ds.R")
source("modules/mod_ae.R")
source("modules/mod_cm.R")


# display each domain in a different page
# NEXT STEP: Make the UI look better using bslib

ui <- navbarPage(
    # make sure to name your seperate page functions mod_dm_ui(id), etc...
    # if you want to test then you can comment out the pages you arent working on
    # (make sure you comment out the server functions you arent testing too)
    title = "Clinical Domains",
    #tabPanel("DM", mod_dm_ui()),
    tabPanel("DS", mod_ds_ui())#,
    #tabPanel("AE", mod_ae_ui()),
    #tabPanel("CM", mod_cm_ui())
    
    # READ THIS: You want another function in each module that will export the ui function
    # mod_dm_ui <- function() {
    #   tagList(
    #   GRAPHS AND OTHER VISUALS GO HERE
  )

