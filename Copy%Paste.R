Create a New Shiny App----> In RStudio:
#     •File → New File → Shiny Web App 
#     •App Name: sales_dashboard 
#     •Choose: Single File (app.R) 
#      Click Create.

install.packages("shinythemes")
#-----------------------------------------------
library(shiny)#web apps 
library(shinydashboard)#dashboard layouts
library(shinythemes)#themes
library(plotly)#interactive charts 
library(ggplot2)
library(dplyr)
library(DT) #interactive tables
library(rsconnect)#deployment
#---------------------------------------------------
setwd("C:/Users/User/OneDrive - University of Ghana/moyussif@NMIMR/NMIMR/4NMIMRdocumnt/mod_r/modibo")
LFparticipants <- read_excel("LF_study.xlsx")

# Sample dataset
data <- mtcars
data$car <- rownames(mtcars)

ui <- dashboardPage(
  
  dashboardHeader(title = "Car Dashboard"),
  
  dashboardSidebar(
    selectInput(
      "xvar",
      "Select X Variable",
      choices = names(data),
      selected = "mpg"
    ),
    
    selectInput(
      "yvar",
      "Select Y Variable",
      choices = names(data),
      selected = "hp"
    )
  ),
  
  dashboardBody(
    
    fluidRow(
      
      box(
        title = "Scatter Plot",
        width = 6,
        plotlyOutput("scatterPlot")
      ),
      
      box(
        title = "Histogram",
        width = 6,
        plotlyOutput("histPlot")
      )
    ),
    
    fluidRow(
      
      box(
        title = "Data Table",
        width = 12,
        DTOutput("table")
      )
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlotly({
    
    p <- ggplot(data,
                aes_string(
                  x = input$xvar,
                  y = input$yvar
                )) +
      geom_point(color = "blue", size = 3) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$histPlot <- renderPlotly({
    
    p <- ggplot(data,
                aes_string(x = input$xvar)) +
      geom_histogram(
        fill = "orange",
        bins = 10
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$table <- renderDT({
    datatable(data)
  })
}

shinyApp(ui, server)
---------------------------------------

#Inside dashboardBody() add another box:
  
  box(
    title = "Bar Chart",
    width = 6,
    plotlyOutput("barPlot")
  )

Then in server:
  
  output$barPlot <- renderPlotly({
    
    p <- ggplot(data,
                aes(x = factor(cyl))) +
      geom_bar(fill = "darkgreen") +
      theme_minimal()
    
    ggplotly(p)
  })
7. Add Reactive Filtering

Add this in sidebar:
  
  sliderInput(
    "hpFilter",
    "Minimum Horsepower",
    min = min(data$hp),
    max = max(data$hp),
    value = min(data$hp)
  )

Create filtered data in server:
  
  filtered_data <- reactive({
    
    data %>%
      filter(hp >= input$hpFilter)
  })

#Then replace data inside plots with:
  
  filtered_data()

  
#Add themes:
Then:

#And inside UI:
  
  fluidPage(
    theme = shinytheme("cerulean")
  )

#More themes:
            cerulean
            flatly
            darkly
            united 
#-------------------------------------------------------------------------------
# To deploy App, Click:“Publish”in RStudio.
#       Choose:
              •	shinyapps.io # Done.
#         You get a public URL like:     https://yourname.shinyapps.io/dashboard
#-------------------------------------------------------------------------------            
  
  
  
  
  
  
  
  
  
  
  