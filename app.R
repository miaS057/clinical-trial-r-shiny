# Load library
library(shiny)
library(DT)
library(tidyverse)
source("ui.R")
source("server.R")


shinyApp(ui = ui, server = server)