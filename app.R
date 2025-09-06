# Load library
library(shiny)
library(tidyverse)

source("ui.R")
source("server.R")


shinyApp(ui = ui, server = server)