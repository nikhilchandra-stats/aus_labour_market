
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


ui <- fluidPage(
  tabsetPanel(
    
    tabPanel(
      
    )
    
  )
  
)

server <- function(input, output) {
  

  
}


shinyApp(ui = ui, server = server)
