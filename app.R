library(ggplot2)
library(stringr)
library(scatterplot3d)
library(shiny)
library(shinyjs)
library(plotly)

smartphone_cpu <- data.frame(read.csv("Smartphone CPU Scores.csv"))
companynames<-c(smartphone_cpu$company)
# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
    # Application title
    titlePanel("SmartPhone Benchmark Scores data"),
    # Dropdown Menu for Different company names
    selectInput("company_name","Please Select Company name:",c("","All",smartphone_cpu$company)),
    selectInput("plots","Please Select Which plot to plot",c("","Bar Plot","Pie Chart")),
    actionButton("out","Plot"),
    actionButton("reset","Clear"),
    ),
    mainPanel(
      textOutput("predict"),
      plotlyOutput("plot")
    )
  )
)
#funcion to plot bar plot..
plotbar<-function(input1,output){
  data<-input1
  if(input1=="All"){
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
  prediction<-data.frame(filter(smartphone_cpu,smartphone_cpu$antutu9 == maxofMax))
  
  output$plot<-renderPlotly({
  fig <- plot_ly(
    x = c("Samsung","Mediatek","Apple","Hisilicon","Unisoc","Qualcomm","Google"),
    y = allMaxValues,
    name = "Smartphone CPU Benchmark Data",
    type = "bar"
  )
  
  fig
  })
  output$predict<-renderText({
    paste("The highest antutu ranking in this data set is of: ",prediction$company," Having the antutu ranking of: ",maxofMax)
  })
  }else{
    compFilter<-data.frame(filter(smartphone_cpu,input1 == smartphone_cpu$company))
    prediction<-data.frame(filter(compFilter,compFilter$antutu9 == max(compFilter$antutu9)))
    output$plot<-renderPlotly({
    fig <- plot_ly(
      x = c(compFilter$cpuName),
      y = compFilter$antutu9,
      name = "Smartphone ranking data",
      type = "bar"
    )
    fig
    })
    output$predict<-renderDataTable({
      prediction
    })
  }
}
#function to plot pie chart..
plotpie<-function(input2,output){
  if(input2=="All"){
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
    output$plot<-renderPlotly({
    pie <- plot_ly(type='pie', labels=c("Samsung","Mediatek","Apple","HiSillicon","Unisoc","Qualcomm","Google"),
                    values=allMaxValues, 
                    textinfo='label+percent',
                    insidetextorientation='radial')
    pie
    })
    predict<-data.frame(filter(smartphone_cpu,smartphone_cpu$antutu9 == maxofMax))
    output$predict<-renderText({
      paste("The highest antutu ranking in this data set is of: ",predict$company," Having the antutu ranking of: ",maxofMax)
    })
    
  }else{
    compFilter<-data.frame(filter(smartphone_cpu,input2 == smartphone_cpu$company))
    output$plot<-renderPlotly({
      pie <- plot_ly(type='pie', labels=c(compFilter$cpuName),
                     values=compFilter$antutu9, 
                     textinfo='label+percent',
                     insidetextorientation='radial')
      pie
    })
  }
}

# Define server logic required to plot both above graphs
server <- function(input, output) {
  listen<-reactive({
    list(input$plots,input$company_name,input$out)
    })
  #observing change in events if any and plotting output..
  observeEvent(listen(),{
    if(input$company_name!=0){
      if(input$plots!=0){
        if(input$out!=0){
        if(input$plots == "Pie Chart"){
          plotpie(input$company_name,output)
        }else if(input$plots == "Bar Plot"){
          plotbar(input$company_name,output)
        }
        }
      }
    }else{
      return(output$text<-renderText({"Please Select Something.."}))
    }
    })
  #Resetting the plot..
  observeEvent(input$reset,{
    output$plot<-renderPlotly({
      
    })
    output$predict<-renderText({
      
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)