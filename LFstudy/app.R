#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
#===========================================================================================================
rsconnect::setAccountInfo(name='moyussif',
                          token='1116A8325821E6841D5828B439A9B93E',
                          secret='<SECRET>')
# =========================
# Load Libraries
# =========================
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
lfstudy_participant <- read_excel("C:\Users\User\OneDrive - University of Ghana\moyussif@NMIMR\NMIMR\4NMIMRdocumnt\mod_r\modibo\lfstudy_participant.xlsx")
sample <-lfstudy_participant
# =========================
# Sample Data
# Replace with your actual dataset
# =========================

research_data <- data.frame(
  record_ID = 1:139,
  site = sample(c("KEEA_Municipal ", "GomoaWest_District"), 139, replace = TRUE),
  keea_facilities = sample(c("Berase CHPS", "Abrem Essiam CHPS", "Han Medical Center","Egyeikrom CHPS", "Abeyee CHPS",
                                          "Nsadwir CHPS", "Aburansa CHPS","Kafodzidzi CHPS", "Kyiase Health Center","Besease CHPS", 
                                          "Kokwaado CHPS","Atonkwa CHPS","Benya CHPS", "Ampenyi CHPS","Brenum Akyinim CHPs",
                                          "Ankwanda CHPS","Antsiambua CHPS", "Benyadzie CHPS"), 139, replace = TRUE),
  keea_communities = sample(c("Berase", "Abrem Essiam", "Bando","Egyeikrom", "Abeyee","Nsadwir", "Aburansa","Kafodzidzi",
                                           "Kyiase","Besease","Kokwaado","Atonkwa","Benya", "Ampenyi","Brenum Akyinim","Ankwanda",
                                           "Antsiambua", "Benyadzie"), 139, replace = TRUE),
  gomoa_west_facilities = sample(c("Ankamu CHPS", "Nkoransa Akwakrom CHPS", "Gomoa Kumasi CHPS","Gomoa Enyeme CHPS",
                                                "Gomoa Akropong", "Sampa CHPS","Asempenyin CHPS", "Gomoa Ngyiresi CHPS",
                                                "A K Debiso CHPS","Tarkwa CHPS","Eshiem CHPS", "Kokofu CHPS","Fomena CHPS",
                                                "Wassa CHPS","Denkyira CHPS","Abonko CHPS","Abrekum CHPS", "Mankwadze CHPS",
                                                "Mprumem CHPS","Assin CHPS","Kyerenkwanta CHPS","Anteadze/Odumase CHPS", "Obiri CHPS",
                                                "Manfro CHPS","Nsuekyir CHPS","Boatyard CHPS","Abankrom CHPS" ), 139, replace = TRUE),
  gomoa_west_communities = sample(c("Ankamu", "Nkoransa Akwakrom", "Gomoa Kumasi","Gomoa Enyeme","Gomoa Akropong", 
                                                 "Sampa","Asempenyin", "Gomoa Ngyiresi","A K Debiso","Tarkwa","Eshiem", "Kokofu",
                                                 "Fomena","Wassa","Denkyira","Abonko","Abrekum", "Mankwadze","Mprumem","Assin",
                                                 "Kyerenkwanta","Anteadze/Odumase", "Obiri","Manfro","Nsuekyir","Boatyard",
                                                 "Abankrom"), 139, replace = TRUE),
  Consent = sample(c("Yes", "No"), 139, replace = TRUE, prob = c(0.9, 0.1)),
  malaria_test_done = sample(c("Yes", "No"), 139, replace = TRUE),
  qfat_test_done = sample(c("Yes", "No"), 139, replace = TRUE),
  malaria_t_results = sample(c("Positive", "Negative"), 139, replace = TRUE),
  qfat_test_result = sample(c("Positive", "Negative"), 139, replace = TRUE),
  age = sample(5:80, 139, replace = TRUE)
)

# =========================
# Dashboard UI
# =========================

ui <- dashboardPage(
  
  dashboardHeader(title = "LF-Study Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("QFAT Analysis", tabName = "qfat", icon = icon("virus")),
      menuItem("Malaria Analysis", tabName = "malaria", icon = icon("hospital")),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # =========================
      # OVERVIEW TAB
      # =========================
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("totalParticipants"),
          valueBoxOutput("consentedParticipants"),
          valueBoxOutput("testedMalaria"),
          valueBoxOutput("testedLF")
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Participants Tested by Facility",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("facilityTestingPlot")
          ),
          
          box(
            width = 6,
            title = "Malaria vs LF Testing",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("malariaLFPlot")
          )
        )
      ),
      
      # =========================
      # FTS TAB
      # =========================
      tabItem(
        tabName = "qfat",
        
        fluidRow(
          box(
            width = 6,
            title = "QFAT Results by Facility",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("qfatFacilityPlot")
          ),
          
          box(
            width = 6,
            title = "QFAT Results by Community",
            status = "danger",
            solidHeader = TRUE,
            plotlyOutput("qfatCommunityPlot")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Age Distribution of QFAT Positive Participants",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("ageQFATPlot")
          )
        )
      ),
      
      # =========================
      # MALARIA TAB
      # =========================
      tabItem(
        tabName = "malaria",
        
        fluidRow(
          box(
            width = 12,
            title = "Malaria Results by Facility",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("malariaFacilityPlot")
          )
        )
      ),
      
      # =========================
      # DATA TABLE TAB
      # =========================
      tabItem(
        tabName = "table",
        
        fluidRow(
          box(
            width = 12,
            title = "LF-Study Dataset",
            status = "info",
            solidHeader = TRUE,
            DTOutput("dataTable")
          )
        )
      )
    )
  )
)


# SERVER

server <- function(input, output) {
  
  output$totalParticipants <- renderValueBox({
    valueBox(
      value = nrow(research_data),
      subtitle = "Total Participants",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$consentedParticipants <- renderValueBox({
    valueBox(
      value = sum(research_data$Consent == "Yes"),
      subtitle = "Participants Consented",
      icon = icon("check"),
      color = "green"
    )
  })
  
  output$testedMalaria <- renderValueBox({
    valueBox(
      value = sum(research_data$malaria_test_done == "Yes"),
      subtitle = "Tested for Malaria",
      icon = icon("vial"),
      color = "yellow"
    )
  })
  
  output$testedLF <- renderValueBox({
    valueBox(
      value = sum(research_data$qfat_test_done == "Yes"),
      subtitle = "Tested for LF",
      icon = icon("virus"),
      color = "red"
    )
  })
  
}


shinyApp(ui, server)   



rsconnect::deployApp() 






# =============================================================================
# LIBRARIES
# =============================================================================
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)

# =========================
# SAMPLE DATA
# =========================

set.seed(123)

research_data <- data.frame(
  record_ID = 1:139,
  
  site = sample(
    c("KEEA_Municipal", "GomoaWest_District"),
    139,
    replace = TRUE
  ),
  
  keea_facilities = sample(
    c(
      "Berase CHPS", "Abrem Essiam CHPS", "Han Medical Center",
      "Egyeikrom CHPS", "Abeyee CHPS", "Nsadwir CHPS",
      "Aburansa CHPS", "Kafodzidzi CHPS",
      "Kyiase Health Center", "Besease CHPS",
      "Kokwaado CHPS", "Atonkwa CHPS",
      "Benya CHPS", "Ampenyi CHPS",
      "Brenum Akyinim CHPS",
      "Ankwanda CHPS", "Antsiambua CHPS",
      "Benyadzie CHPS"
    ),
    139,
    replace = TRUE
  ),
  
  keea_communities = sample(
    c(
      "Berase", "Abrem Essiam", "Bando",
      "Egyeikrom", "Abeyee", "Nsadwir",
      "Aburansa", "Kafodzidzi", "Kyiase",
      "Besease", "Kokwaado", "Atonkwa",
      "Benya", "Ampenyi", "Brenum Akyinim",
      "Ankwanda", "Antsiambua", "Benyadzie"
    ),
    139,
    replace = TRUE
  ),
  
  gomoa_west_facilities = sample(
    c(
      "Ankamu CHPS", "Nkoransa Akwakrom CHPS",
      "Gomoa Kumasi CHPS", "Gomoa Enyeme CHPS",
      "Gomoa Akropong", "Sampa CHPS",
      "Asempenyin CHPS", "Gomoa Ngyiresi CHPS",
      "A K Debiso CHPS", "Tarkwa CHPS",
      "Eshiem CHPS", "Kokofu CHPS",
      "Fomena CHPS", "Wassa CHPS",
      "Denkyira CHPS", "Abonko CHPS",
      "Abrekum CHPS", "Mankwadze CHPS",
      "Mprumem CHPS", "Assin CHPS",
      "Kyerenkwanta CHPS",
      "Anteadze/Odumase CHPS",
      "Obiri CHPS", "Manfro CHPS",
      "Nsuekyir CHPS", "Boatyard CHPS",
      "Abankrom CHPS"
    ),
    139,
    replace = TRUE
  ),
  
  gomoa_west_communities = sample(
    c(
      "Ankamu", "Nkoransa Akwakrom",
      "Gomoa Kumasi", "Gomoa Enyeme",
      "Gomoa Akropong", "Sampa",
      "Asempenyin", "Gomoa Ngyiresi",
      "A K Debiso", "Tarkwa",
      "Eshiem", "Kokofu",
      "Fomena", "Wassa",
      "Denkyira", "Abonko",
      "Abrekum", "Mankwadze",
      "Mprumem", "Assin",
      "Kyerenkwanta",
      "Anteadze/Odumase",
      "Obiri", "Manfro",
      "Nsuekyir", "Boatyard",
      "Abankrom"
    ),
    139,
    replace = TRUE
  ),
  
  Consent = sample(
    c("Yes", "No"),
    139,
    replace = TRUE,
    prob = c(0.9, 0.1)
  ),
  
  malaria_test_done = sample(
    c("Yes", "No"),
    139,
    replace = TRUE
  ),
  
  qfat_test_done = sample(
    c("Yes", "No"),
    139,
    replace = TRUE
  ),
  
  malaria_t_results = sample(
    c("Positive", "Negative"),
    139,
    replace = TRUE
  ),
  
  qfat_test_result = sample(
    c("Positive", "Negative"),
    139,
    replace = TRUE
  ),
  
  age = sample(5:80, 139, replace = TRUE)
)

# =========================
# UI
# =========================

ui <- dashboardPage(
  
  dashboardHeader(title = "LF-Study Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("QFAT Analysis", tabName = "qfat", icon = icon("virus")),
      menuItem("Malaria Analysis", tabName = "malaria", icon = icon("hospital")),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("totalParticipants"),
          valueBoxOutput("consentedParticipants"),
          valueBoxOutput("testedMalaria"),
          valueBoxOutput("testedLF")
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Participants Tested by Facility",
            plotlyOutput("facilityTestingPlot")
          ),
          
          box(
            width = 6,
            title = "Malaria vs LF Testing",
            plotlyOutput("malariaLFPlot")
          )
        )
      ),
      
      tabItem(
        tabName = "qfat",
        
        fluidRow(
          box(
            width = 6,
            title = "QFAT Results by Facility",
            plotlyOutput("qfatFacilityPlot")
          ),
          
          box(
            width = 6,
            title = "QFAT Results by Community",
            plotlyOutput("qfatCommunityPlot")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Age Distribution",
            plotlyOutput("ageQFATPlot")
          )
        )
      ),
      
      tabItem(
        tabName = "malaria",
        
        fluidRow(
          box(
            width = 12,
            title = "Malaria Results by Facility",
            plotlyOutput("malariaFacilityPlot")
          )
        )
      ),
      
      tabItem(
        tabName = "table",
        
        fluidRow(
          box(
            width = 12,
            title = "Dataset",
            DTOutput("dataTable")
          )
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output) {
  
  output$totalParticipants <- renderValueBox({
    valueBox(nrow(research_data), "Total Participants",
             icon = icon("users"), color = "blue")
  })
  
  output$consentedParticipants <- renderValueBox({
    valueBox(sum(research_data$Consent == "Yes"),
             "Participants Consented",
             icon = icon("check"),
             color = "green")
  })
  
  output$testedMalaria <- renderValueBox({
    valueBox(sum(research_data$malaria_test_done == "Yes"),
             "Tested for Malaria",
             icon = icon("vial"),
             color = "yellow")
  })
  
  output$testedLF <- renderValueBox({
    valueBox(sum(research_data$qfat_test_done == "Yes"),
             "Tested for LF",
             icon = icon("virus"),
             color = "red")
  })
  
  # Facility plot
  output$facilityTestingPlot <- renderPlotly({
    
    p <- research_data %>%
      count(keea_facilities) %>%
      plot_ly(
        x = ~keea_facilities,
        y = ~n,
        type = "bar"
      )
    
    p
  })
  
  # Malaria vs LF
  output$malariaLFPlot <- renderPlotly({
    
    test_counts <- data.frame(
      Test = c("Malaria", "LF"),
      Count = c(
        sum(research_data$malaria_test_done == "Yes"),
        sum(research_data$qfat_test_done == "Yes")
      )
    )
    
    plot_ly(
      test_counts,
      x = ~Test,
      y = ~Count,
      type = "bar"
    )
  })
  
  # QFAT facility plot
  output$qfatFacilityPlot <- renderPlotly({
    
    qfat_data <- research_data %>%
      filter(qfat_test_result == "Positive") %>%
      count(keea_facilities)
    
    plot_ly(
      qfat_data,
      x = ~keea_facilities,
      y = ~n,
      type = "bar"
    )
  })
  
  # QFAT community plot
  output$qfatCommunityPlot <- renderPlotly({
    
    qfat_comm <- research_data %>%
      filter(qfat_test_result == "Positive") %>%
      count(keea_communities)
    
    plot_ly(
      qfat_comm,
      labels = ~keea_communities,
      values = ~n,
      type = "pie"
    )
  })
  
  # Age histogram
  output$ageQFATPlot <- renderPlotly({
    
    positive_data <- research_data %>%
      filter(qfat_test_result == "Positive")
    
    plot_ly(
      positive_data,
      x = ~age,
      type = "histogram"
    )
  })
  
  # Malaria facility plot
  output$malariaFacilityPlot <- renderPlotly({
    
    malaria_data <- research_data %>%
      filter(malaria_t_results == "Positive") %>%
      count(keea_facilities)
    
    plot_ly(
      malaria_data,
      x = ~keea_facilities,
      y = ~n,
      type = "bar"
    )
  })
  
  # Data table
  output$dataTable <- renderDT({
    datatable(research_data)
  })
  
}

# =========================
# RUN APP
# =========================

shinyApp(ui, server)





