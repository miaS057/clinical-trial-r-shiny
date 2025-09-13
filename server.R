# Load libraries
library(shiny)

# For now keep just use server.R with pharmaversesdtm
# Later on we can switch it so they can import a file from user input on the UI

if (!requireNamespace("pharmaversesdtm")) {
  install.packages("pharmaversesdtm")
}
library(pharmaversesdtm)


server <- function(input, output, session) {
  
  # load in each dataset
  data("dm", package = "pharmaversesdtm")
  data("ds", package = "pharmaversesdtm")
  data("ae", package = "pharmaversesdtm")
  data("cm", package = "pharmaversesdtm")
  
  # make them reactive so we can import them into each module
  dm_r <- reactive(dm)
  ds_r <- reactive(ds)
  ae_r <- reactive(ae)
  cm_r <- reactive(cm)
  
  # NOTE: if you have certain data mutations used in more than one module
  #       put them in this global server
  
  # we need to export the data we need from the server to each module
  # NOTE: So for each module we need to have a server component
  
  #mod_dm_server("dm", dm_r = dm_r)
  
  # NOTE: if your particular component needs other data_sets
  # you can add them here
  #mod_ds_server(dm_r = dm_r, ds_r = ds_r)
  #mod_ae_server(dm_r = dm_r, ae_r = ae_r)
  mod_cm_server(dm_r = dm_r, cm_r = cm_r, output = output, input = input)
  
  # READ THIS: So in each module you can get the data from the server by doing
  # mod_dm_server <- function(dm_r) {
  #     CALL DATA USING THE REACTIVE
  #     Example: ds <- ds_r()
  #
  #     CODE GOES HERE
  
}



