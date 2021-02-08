
library("tidyverse")
library("ggplot2")
library("lubridate")
library("janitor")
library("jsonlite")
library("gghighlight")
library("ggrepel")
library("ggthemes")
library(shiny)
library(shinyjs)
library(pins)

source("detailed_plots.R")

load("abs_data.RData")

# .data <- returned_data[[37]] %>%
#   pivot_longer(-c(month_date, sex, state,industry),values_to = "value", names_to = "employment_or_hours") 

main_plot_premade <- returned_data[[37]] %>%
  pivot_longer(-c(month_date, sex, state,industry),values_to = "value", names_to = "employment_or_hours") %>%
  split(.$state) %>%
  purrr::map( ~ detailed_plot_forecast(.x) )

faceted_comp_plot <- returned_data[[37]] %>%
  pivot_longer(-c(month_date, sex, state,industry),values_to = "value", names_to = "employment_or_hours") %>%
  detailed_plot_forecast()

ui <- fluidPage(
  
  mainPanel(
  
  tabsetPanel(
    
    tabPanel(
      "Detailed Labour market View",
          plotOutput("all_plot1",width = 1200,height = 900),
          plotOutput("all_plot2",width = 800,height = 900)          
      
    ), 
    
    tabPanel(
      "State Detailed View",
      plotOutput("QLD_plot1",width = 800,height = 900),
      plotOutput("NSW_plot1",width = 800,height = 900),
      
    )
    
  )
  )
)

server <- function(input, output) {

output$all_plot1 <- renderPlot({
    
  faceted_comp_plot[[1]]
    
})

output$all_plot2 <- renderPlot({
  
  faceted_comp_plot[[2]]
  
})
    
output$QLD_plot1 <- renderPlot({
  
  main_plot_premade[[4]][[2]]

})  

output$NSW_plot1 <- renderPlot({
  
  main_plot_premade[[2]][[2]]
  
})  
  
}


shinyApp(ui = ui, server = server)
