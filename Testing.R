library(readxl)
library(rsconnect)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
#import utils.R
source("utils.R")
#import ui.R and server.R
source("ui.R")
source("server.R")
#Run the application
shinyApp(ui = ui, server = server)
LF_participant <- read_excel("RABBI/LF_participant.xlsx")