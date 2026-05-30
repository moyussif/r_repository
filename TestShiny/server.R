#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
LF_participant <- read_excel("TestShiny/LF_participant.xlsx")

# Define server logic required to draw a histogram
plot_records <- function(TestShiny) {
  
 total_participants <- TestShiny %>%
   summarize(Number_attended = sum(record_id, na.rm = TRUE)) %>% 
   pull(Number_attended)
 plot <- plot_ly(
   x = "Number_attended",
   y = total_participants,
   type = "bar",
   text = ~paste0("$", format(total_participants, big.mark = " ")),
   marker = list(color = "darkorange")
 ) %>% 
   layout(
     title = "Number_attended",
     xaxis = list(title = " "),
     yaxis = list(title = "Number_attended"),
     bargap = 0.2
   )
 return(plot)
 
}
