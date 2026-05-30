#Best for now

library(readxl)
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
#--------------------------------------------------------------------------------------------------------
header <- dashboardHeader(title = "me trying this")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  fluidRow(
    valueBoxOutput("record_id", width = 4),
    valueBoxOutput("consent", width = 4),
    valueBoxOutput("malaria_test_done", width = 4),
    valueBoxOutput("qfat_test_done", width = 4)
  ),
  fluidRow(
   
  ),
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
}

shinyApp(ui, server)