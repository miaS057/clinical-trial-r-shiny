library(bslib)
library(shiny)
library(DT)

source("modules/domain_style.R")
source("modules/mod_dm.R")
source("modules/mod_ds.R")
source("modules/mod_ae.R")
source("modules/mod_cm.R")

ui <- navbarPage(
  title = "Clinical Domains",
  id = "clinical_domain",
  theme = bslib::bs_theme(version = 5, bg = "#f6f8fb", fg = "#24364b", primary = "#266794"),
  header = tags$head(tags$link(rel = "stylesheet", href = "dashboard.css")),
  tabPanel("DM", mod_dm_ui()),
  tabPanel("DS", mod_ds_ui()),
  tabPanel("AE", mod_ae_ui()),
  tabPanel("CM", mod_cm_ui())
)
