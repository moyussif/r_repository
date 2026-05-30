
#
setwd("C:/Users/User/OneDrive - University of Ghana/moyussif@NMIMR/NMIMR/4NMIMRdocumnt/mod_r/modibo/LF_Dashboard")
library(rsconnect)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)

shiny::runApp()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("LF Study_Participant form"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "site",
                        label = "Choose site", "Name",
                        min = 1,
                        max =10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  LFdata <- read_excel("LF_Dashboard/LF_participant.xlsx")

  #Summarize Data and then plot
LF_data <- reactive({
  req(input$site)
  df <- LFdata %>% filter(site %in% input$site) %>% group_by(keea_facilities) %>% summarise(Participants = sum(read_id))
})
#plot
output$plot <-renderPlot({
  g <- ggplot(LF_data(), aes(y = Participants, x = keea_facilities))
  g + geom_bar(stat = "sum")
})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
