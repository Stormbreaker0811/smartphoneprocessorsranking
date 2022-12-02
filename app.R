?sidebarLayout
library(ggplot2)
library(stringr)
library(scatterplot3d)
library(shiny)
library(plotly)
ml_all<-data.frame(read.csv("ml_all.csv"))
smartphone_cpu = data.frame(read.csv("Smartphone CPU Scores.csv"))
companynames<-c(smartphone_cpu$company)
company<-table(smartphone_cpu$company)
company
arr<-as.character(company)
arr

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
    # Application title
    titlePanel("SmartPhone Benchmark Scores data"),
    # Dropdown Menu for Different company names
    selectInput("company_name","Please Select Company name:",smartphone_cpu$company)
    ),
    mainPanel(
      dataTableOutput("tables")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  company_name<-reactive({input$company_name})
  observeEvent(input$company_name,{
      for(x in input$company_name){
        output$tables<-renderDataTable({
          filter(smartphone_cpu,x == smartphone_cpu$company)
        })
      }
    })
    #pie(as.integer(arr),smartphone_cpu$company)
}

# Run the application 
shinyApp(ui = ui, server = server)
