source("run_all_v2.R")
source("get_all_labour_gender.R")
source("plot_summaries.R")
source("ABS_eco_data2.R")
source("labour_by_age_detailed.R")
library("shiny")

#labour_data_cubes <- get_data_local_gender()
labour_data <- eco_tables_abs()
library(shiny)
library(shinyjs)
#-------------------------------------------------------Cluster map
data_download1 <- labour_data[[28]][[2]] %>%
  downloaded_data_national()

age_groups <- labour_data[[29]][[2]] %>%
  distinct(age)

ui <- fluidPage(
  mainPanel(
    h3( "Australian Labour Market - Women's Economic Security" ),
    tabsetPanel(
      
      tabPanel(
        "National Labour Market View",
        h5("(Page may take a few minutes to load data and visualisations)"),
        plotOutput("plot_main_nat",height = 900,width = 1000),
        h6("Source: ABS Labour Force 6202 Table 23"),
        h6("Download Underlying Data"),
        sidebarPanel(
          shiny::downloadButton(outputId = "download_data1")
        ) 
      ),
      
      tabPanel(
        "Industry Labour Market View",
        h5("(Page may take a few minutes to load data and visualisations)"),
        
        sidebarPanel(
          
          shiny::selectInput(inputId = "people_hours",label = "Analysis Metric\n(Hours Worked,People)",
                             choices = c("hours","people"),selected = "people"  )
        ),
          mainPanel(plotOutput("plot_main_indus",height = 900,width = 900)),
        
        h6("Source: ABS Labour Force Detailed 6291.0 tables EQ09"),
        h6("Download Underlying Data"),
        shiny::downloadButton(outputId = "download_data2")
      ),
      

      tabPanel(
        "Detailed Labour Market View (by Age)",
        h5("(Page may take a few minutes to load data and visualisations)"),
        plotOutput("plot_age",height = 1300,width = 1200),
        h6("Source: ABS Labour Force Detailed 6202.0 tables 24"),
        h6("Download Underlying Data"),
        shiny::downloadButton(outputId = "download_data3")
      )

      
    )
  )
)

server <- function(input, output) {
  
  output$plot_main_indus <- renderPlot({
    labour_data[[31]][[2]] %>%
      plot_summary_indus(people_hours = input$people_hours)
  })
  
  output$plot_main_nat <- renderPlot({
    labour_data[[28]][[2]] %>%
      plot_summary_1()
  })
  
  output$plot_age <- renderPlot({
    labour_data[[29]][[2]] %>%
      plot_by_age()

  })

  output$download_data1 <- downloadHandler(
    
    filename = function(){
      "Gender_breakdown_summary.csv"
    }, 
    content = function(file) {
      write.csv(data_download1, file)
    }
  )
  
  output$download_data2 <- downloadHandler(
    
    filename = function(){
      "Gender_breakdown_industry_summary.csv"
    }, 
    content = function(file) {
      
      data_download2 <- labour_data[[31]][[2]] %>%
        plot_summary_indus_download(people_hours = input$people_hours)
      
      write.csv(data_download2, file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
