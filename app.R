
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

load("abs_data.RData")
.data <- returned_data[[37]] %>%
  pivot_longer(-c(month_date, sex, state,industry),values_to = "value", names_to = "employment_or_hours") 

main_plot_premade <- returned_data[[37]] %>%
  pivot_longer(-c(month_date, sex, state,industry),values_to = "value", names_to = "employment_or_hours") %>%
  split(.$state) %>%
  purrr::map( ~ detailed_plot_forecast(.x) )

ui <- fluidPage(
  tabsetPanel(
    
    tabPanel(
      plotOutput("main_plot",width = 1920,height = 1080)
    )
    
  )
  
)

server <- function(input, output) {
  
output$main_plot <- renderPlot({
  
  main_plot_premade[[2]]

})  
  
  
}


shinyApp(ui = ui, server = server)
