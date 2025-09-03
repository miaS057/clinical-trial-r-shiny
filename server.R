# Load libraries

# For now keep just use server.R with pharmaversesdtm
# Later on we can switch it so they can import a file from user input on the UI



if (!requireNamespace("pharmaverse")) {
  install.packages("pharmaverse")
}
library(pharmaverse)


server <- function(input, output, session) {
  
  # load in each dataset
  data("dm", package = "pharmaversesdtm")
  data("ds", package = "pharmaversesdtm")
  data("ae", package = "pharmaversesdtm")
  data("cm", package = "pharmaversesdtm")
  
  # make them reactive so we can import them into each module
  dm_r <- reactive(dm_data)
  ds_r <- reactive(ds_data)
  ae_r <- reactive(ae_data)
  cm_r <- reactive(cm_data)
  
  
  # we need to export the data we need from the server to each module
  # NOTE: So for each module we need to have a server component
  
  mod_dm_server("dm", dm_r = dm_r)
  
  # NOTE: if your particular component needs other data_sets
  # you can add them here
  mod_ds_server("ds", dm_r = dm_r, ds_r = ds_r)
  mod_ae_server("ae", dm_r = dm_r, ae_r = ae_r)
  mod_cm_server("cm", dm_r = dm_r, cm_r = cm_r)
}
