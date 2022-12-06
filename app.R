library(ggplot2)
library(stringr)
library(scatterplot3d)
library(shiny)
library(plotly)

smartphone_cpu <- data.frame(read.csv("Smartphone CPU Scores.csv"))

companynames<-c(smartphone_cpu$company)
#company<-table(smartphone_cpu$company)
#company
#arr<-as.character(company)
#arr
barPlot<-function(companyfilter){
  if(companyfilter == "All"){
    samsung<-data.frame(filter(smartphone_cpu,smartphone_cpu$company == "Samsung"))
    mediatek<-data.frame(filter(smartphone_cpu,smartphone_cpu$company == "MediaTek"))
    apple<-data.frame(filter(smartphone_cpu,smartphone_cpu$company == "Apple"))
    hisilicon<-data.frame(filter(smartphone_cpu,smartphone_cpu$company == "HiSilicon"))
    unisoc<-data.frame(filter(smartphone_cpu,smartphone_cpu$company == "Unisoc"))
    qualcomm<-data.frame(filter(smartphone_cpu,smartphone_cpu$company == "Qualcomm"))
    google<-data.frame(filter(smartphone_cpu,smartphone_cpu$company == "Google"))
    allMaxValues<-c(max(samsung$antutu9),max(mediatek$antutu9),max(apple$antutu9),
                    max(hisilicon$antutu9),max(unisoc$antutu9),max(qualcomm$antutu9),
                    max(google$antutu9))
    maxofMax<-max(allMaxValues)
    maxofMax
    fig <- plot_ly(
      x = c("Samsung", "Mediatek", "Apple","HiSilicon", "Unisoc", "Qualcomm","Google"),
      y = as.integer(allMaxValues),
      name = "Smartphone CPU Benchmark Data",
      type = "bar"
    )
    
    fig
  }else{
    compFilter<-data.frame(filter(smartphone_cpu,companyfilter == smartphone_cpu$company))
    fig <- plot_ly(
      #x = c(compFilter$cpuName),
      y = as.integer(compFilter$antutu9),
      name = "Smartphone CPU Benchmark Data",
      type = "bar"
    )
    
    fig
  }
}
# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
    # Application title
    titlePanel("SmartPhone Benchmark Scores data"),
    # Dropdown Menu for Different company names
    selectInput("company_name","Please Select Company name:",c("All",smartphone_cpu$company))
    ),
    mainPanel(
      actionButton("piechart","Plot Pie Chart"),
      actionButton("barPlot","Plot Bar Plot"),
      plotlyOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ?aes
  company_name<-reactive({input$company_name})
  observeEvent(input$company_name,{
      output$plot<-renderPlotly({
        barplot(input$company_name)
        #fig <- plot_ly(y=companyfilter$antutu9, x=companyfilter$cpuName, histfunc='sum', type = "histogram")
        #fig <- fig %>% layout(yaxis=list(type='linear'))
        #pie(companyfilter$antutu9,companyfilter$cpuName,diameter = 1)
      })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
