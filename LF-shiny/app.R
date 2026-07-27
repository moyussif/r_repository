#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(bslib)
library(dplyr)
library(gtsummary)
library(gt)
library(DT)
library(plotly)
library(ggplot2)
library(patchwork)

# Define UI for application that draws a histogram
ui <- page_sidebar(
  
  title = "LF Surveillance Dashboard",
  
  sidebar = sidebar(
    
    selectInput(
      "site",
      "Site",
      choices = c("All", unique(df$site)),
      selected = "All"
    ),
    
    selectInput(
      "sex",
      "Sex",
      choices = c("All", unique(df$sex)),
      selected = "All"
    ),
    
    selectInput(
      "age",
      "Age group",
      choices = c("All", unique(df$Age_group)),
      selected = "All"
    )
    
  ),
  
  navset_card_tab(
    
    nav_panel(
      "Summary",
      
      gt_output("basic")
    ),
    
    nav_panel(
      "Malaria",
      
      gt_output("malaria")
    ),
    
    nav_panel(
      "FTS",
      
      gt_output("fts")
    ),
    
    nav_panel(
      "Prevalence",
      
      plotlyOutput("prev_plot", height = 700)
    ),
    
    nav_panel(
      "Community Results",
      
      DTOutput("community")
    )
    
  )
)

# ================================= Define server logic required to draw a histogram ====================
server <- function(input, output){
  
  data_filtered <- reactive({
    
    x <- df
    
    if(input$site != "All")
      x <- filter(x, site == input$site)
    
    if(input$sex != "All")
      x <- filter(x, sex == input$sex)
    
    if(input$age != "All")
      x <- filter(x, Age_group == input$age)
    
    x
    
  })
#-------------------------------------------------------------------------------
#--------Basic Summary
  output$basic <- render_gt({
    
    data_filtered() %>%
      tbl_summary(
        include = c(consent, site),
        statistic = all_categorical() ~ "{n} ({p}%)",
        missing = "ifany"
      ) %>%
      bold_labels() %>%
      italicize_levels() %>%
      modify_caption("**Basic Summary**") %>%
      as_gt()
    
  })
  #### Malaria Summary
  output$malaria <- render_gt({
    
    data_filtered() %>%
      tbl_summary(
        include = c(
          malaria_test_done,
          malaria_t_results
        ),
        statistic = all_categorical() ~ "{n} ({p}%)",
        missing = "ifany"
      ) %>%
      bold_labels() %>%
      italicize_levels() %>%
      modify_caption("**Malaria Results**") %>%
      as_gt()
    
  })
  ##### FTS Summary
  output$fts <- render_gt({
    
    data_filtered() %>%
      tbl_summary(
        include = c(
          fts_test_done,
          fts_test_result
        ),
        statistic = all_categorical() ~ "{n} ({p}%)",
        missing = "ifany"
      ) %>%
      bold_labels() %>%
      italicize_levels() %>%
      modify_caption("**FTS Results**") %>%
      as_gt()
    
  })
  #### Interactive prevalence plot
  output$prev_plot <- renderPlotly({
    
    prev <- data_filtered() %>%
      
      group_by(site, sex, Age_group) %>%
      
      summarise(
        Prev = mean(fts_test_result=="Positive", na.rm=TRUE)*100,
        .groups="drop"
      )
    
    p <-
      
      ggplot(prev,
             aes(
               Age_group,
               Prev,
               fill = sex,
               text = paste(
                 "Age:",Age_group,
                 "<br>Sex:",sex,
                 "<br>Prev:",round(Prev,1),"%"
               )
             ))+
      
      geom_col(position="dodge")+
      
      facet_wrap(~site)+
      
      labs(
        x="Age group",
        y="Prevalence (%)"
      )+
      
      theme_minimal()
    
    ggplotly(p, tooltip="text")
    
  })
  ##### Interactive community table
  output$community <- renderDT({
    
    data_filtered() %>%
      
      group_by(site,
               keea_communities,
               gomoa_west_communities) %>%
      
      summarise(
        
        Malaria_Tested=n(),
        
        Malaria_Positive=
          sum(malaria_t_results=="Positive",na.rm=TRUE),
        
        FTS_Tested=
          sum(!is.na(fts_test_result)),
        
        FTS_Positive=
          sum(fts_test_result=="Positive",na.rm=TRUE),
        
        .groups="drop"
        
      ) %>%
      
      datatable(
        
        filter="top",
        
        extensions="Buttons",
        
        options=list(
          
          pageLength=20,
          
          dom="Bfrtip",
          
          buttons=c("copy","csv","excel","pdf","print")
          
        )
        
      )
    
  })
  #####
  shinyApp(ui, server)
  
# ================================ Run the application =========================================
shinyApp(ui = ui, server = server)
