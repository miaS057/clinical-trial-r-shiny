# Load library
library(shiny)
library(tidyverse)
library(plotly)

source("ui.R")
source("server.R")


shinyApp(ui = ui, server = server)