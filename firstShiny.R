library(readxl)
immunoData <- read_excel("C:/Users/User/Desktop/repos/immunoData.xlsx")
View(immunoData)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Immunodata"),
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
           plotOutput("distPlot"),
           plotOutput("scatter")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
         x   <- immunoData$age
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'blue', border = 'white',
             xlab = 'age',
             main = 'mother age')
    })
    output$scatter <- renderPlot({
       y <- immunoData$age
       x <- immunoData$bmi
         plot(y~x,xlab = 'Age', ylab = 'Bmi', main = 'Age by Bmi', col = 'green')
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
