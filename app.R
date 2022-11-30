library(ggplot2)
library(scatterplot3d)
library(shiny)
antutu<-read.csv("antutu_android_vs_ios_v4.csv")
ml_all<-read.csv("ML_ALL_benchmarks.csv")
smartphone_cpu<-read.csv("smartphone_cpu_stats.csv")
dataset<-list('antutu scores' = antutu,'Smartphone CPU Scores' = smartphone_cpu,
              'ML_ALL' = ml_all)
# Define UI for application that draws a histogram
ui <- fluidPage(
    selectInput("SBS","Choose A File",names(dataset)),
    actionButton("read_file","Read Chosen File"),
    # Application title
    titlePanel("SmartPhone Benchmark Scores data"),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  sbs<-reactive({input$SBS})
  sbsbutton<-reactive({input$read_file})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
