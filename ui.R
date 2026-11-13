library(bslib)
library(shiny)
library(DT)

source("data.R")
source("modules/domain_style.R")
source("modules/mod_cm.R")
source("modules/mod_dm.R")
source("modules/mod_ds.R")
source("modules/mod_ae.R")

ui <- navbarPage(
  title = NULL,
  windowTitle = "Clinical Trial Dashboard",
  id = "clinical_domain",
  theme = bslib::bs_theme(version = 5, bg = "#f6f8fb", fg = "#24364b", primary = "#266794"),
  header = tags$head(tags$link(rel = "stylesheet", href = "dashboard.css")),
  tabPanel(
    "Home",
    div(
      class = "trial-home",
      div(
        class = "trial-home-title",
        h1("Clinical Trial Dashboard")
      ),
      div(
        class = "trial-home-upload",
        h2("Import study data"),
        tags$p("DM is required. Add any other domains from the same study as CSV or SAS XPT files."),
        div(
          class = "trial-file-grid",
          lapply(names(sdtm_labels), function(domain) {
            id <- paste0("sdtm_", domain)
            div(
              class = "trial-file-picker",
              uiOutput(paste0(id, "_selection"))
            )
          })
        ),
        div(
          class = "trial-import-actions",
          actionButton("sdtm_import", "Load study", class = "btn-primary", disabled = TRUE),
          conditionalPanel(
            "output.sdtm_loaded",
            actionButton("sdtm_reset", "Reset", class = "btn-outline-secondary")
          )
        ),
        uiOutput("sdtm_status")
      )
    )
  ),
  tabPanel("DM", mod_dm_ui()),
  tabPanel("DS", mod_ds_ui()),
  tabPanel("AE", mod_ae_ui()),
  tabPanel("CM", mod_cm_ui())
)
