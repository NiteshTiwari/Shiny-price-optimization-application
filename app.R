## app.R ##
library(shiny)
library(dplyr)
library(shinydashboard)
options(shiny.maxRequestSize=30*1024^2) 

ui <- dashboardPage(
  dashboardHeader(title = "LAZ Parking Ticket Price Optimization", titleWidth = 400),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      tags$head(tags$style(".wrapper {overflow: visible !important;}")),
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
     menuItem("Timeband Revenues", tabName = "elastic", icon = icon("dashboard")),
     menuItem("Price Elasticity", tabName = "dataset", icon = icon("dashboard"))
    ),
    helpText("Choose slider to increase or", 
             "decrease the Ticket rate ",
             "in specific Timebands (%change)"),
    sliderInput("band1", label = ("Weekday/before 4pm/0-20min"),
                min = 0, max = 2, value = 1, step= 0.1),
    sliderInput("band2", label = ("Weekday/before 4pm/20-40min"),
                min = 0, max = 2, value = 1, step= 0.1),
    sliderInput("band3", label = ("Weekday/before 4pm/40-60min"),
                min = 0, max = 2, value = 1, step= 0.1),
    sliderInput("band4", label = ("Weekday/before 4pm/60-80min"),
                min = 0, max = 2, value = 1, step= 0.1),
    sliderInput("band5", label = ("Weekday/before 4pm/Over 80min"),
                min = 0, max = 2, value = 1, step= 0.1),
    sliderInput("band6", label = ("Weekday/After 4pm-6am"),
                min = 0, max = 2, value = 1, step= 0.1),
    sliderInput("band7", label = ("Weekend/6am-6am"),
                min = 0, max = 2, value = 1, step= 0.1),
    sliderInput("sensitivity7", label = ("Weekend/6am-6am/sensitivity7"),
                min = 0, max = 2, value = 1, step= 0.1)
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dataset",
              fluidRow(
                tabBox(
                  title = "Price Elasticity Model",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("Dataset", dataTableOutput('dataset'))
                  
    
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "elastic",
              fluidRow(
                column(width =4,
                sliderInput("sensitivity1", label = ("Weekday/before 4pm/0-20min/sensitivity"),
                            min = 0, max = 2, value = 1, step= 0.1),
                sliderInput("sensitivity2", label = ("Weekday/before 4pm/20-40min/sensitivity"),
                            min = 0, max = 2, value = 1, step= 0.1)
                ),
                column(width=4,
                sliderInput("sensitivity3", label = ("Weekday/before 4pm/40-60min/sensitivity"),
                            min = 0, max = 2, value = 1, step= 0.1),
                sliderInput("sensitivity4", label = ("Weekday/before 4pm/60-80min/sensitivity"),
                            min = 0, max = 2, value = 1, step= 0.1)
                ),
                column(width =4,
                sliderInput("sensitivity5", label = ("Weekday/before 4pm/Over 80min/sensitivity"),
                            min = 0, max = 2, value = 1, step= 0.1),
                sliderInput("sensitivity6", label = ("Weekday/After 4pm-6am/sensitivity"),
                            min = 0, max = 2, value = 1, step= 0.1)
                ),
               infoBoxOutput("weekday9"),
               infoBoxOutput("weekday9elastic"),
               infoBoxOutput("weekday9diff"),
               infoBoxOutput("weekday19"),
               infoBoxOutput("weekday19elastic"),
               infoBoxOutput("weekday19diff"),
               infoBoxOutput("weekday29"),
               infoBoxOutput("weekday29elastic"),
               infoBoxOutput("weekday29diff"),
               infoBoxOutput("weekday36"),
               infoBoxOutput("weekday36elastic"),
               infoBoxOutput("weekday36diff"),
               infoBoxOutput("weekday41"),
               infoBoxOutput("weekday41elastic"),
               infoBoxOutput("weekday41diff"),
               infoBoxOutput("weeknight13"),
               infoBoxOutput("weeknight13elastic"),
               infoBoxOutput("weeknight13diff"),
               infoBoxOutput("weekend"),
               infoBoxOutput("weekendelastic"),
               infoBoxOutput("weekenddiff"),
               infoBoxOutput("total"),
               infoBoxOutput("totalelastic"),
               infoBoxOutput("totaldiff")
              )
      )
    )
  )
)

server <- function(input, output) { 
  
  data<-reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    na.omit(read.csv(inFile$datapath))
    
  })
  #glmoutputs
  output$dataset<-renderDataTable({select(data(),PaymentNumber, TranDateTime, DayOfWeek,DayType,duration,Discounted,
                                          NetPrice,NetTurnover,TimeBand)},
                                          options = list(lengthMenu = c(5, 10, 20), pageLength = 20))
  

  
  output$weekday9 <- renderInfoBox({
    infoBox(
      "Weekday/0-20", sum(data()[which(data()$TimeBand=="Weekday9"),]$NetPrice), icon = icon("th-list", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$weekday9elastic <- renderInfoBox({
    infoBox(
      "Weekday/0-20/elastic", (sum(data()[which(data()$TimeBand=="Weekday9"),]$NetPrice)*input$band1)/(input$sensitivity1), icon = icon("indent-left", lib = "glyphicon"),
      color = "green"
    )
  })
  output$weekday9diff <- renderInfoBox({
    infoBox(
      "Weekday/0-20/diff", ((sum(data()[which(data()$TimeBand=="Weekday9"),]$NetPrice)*input$band1)/(input$sensitivity1)-sum(data()[which(data()$TimeBand=="Weekday9"),]$NetPrice)), icon = icon("usd", lib = "glyphicon"),
      color = "purple"
    )
  })
  output$weekday19 <- renderInfoBox({
    infoBox(
      "Weekday/20-40", sum(data()[which(data()$TimeBand=="Weekday19"),]$NetPrice), icon = icon("th-list", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$weekday19elastic <- renderInfoBox({
    infoBox(
      "Weekday/20-40/elastic", (sum(data()[which(data()$TimeBand=="Weekday19"),]$NetPrice)*input$band2)/(input$sensitivity2), icon = icon("indent-left", lib = "glyphicon"),
      color = "green"
    )
  })
  output$weekday19diff <- renderInfoBox({
    infoBox(
      "Weekday/20-40/diff", ((sum(data()[which(data()$TimeBand=="Weekday19"),]$NetPrice)*input$band2/(input$sensitivity2))-sum(data()[which(data()$TimeBand=="Weekday19"),]$NetPrice)), icon = icon("usd", lib = "glyphicon"),
      color = "purple"
    )
  })
  output$weekday29 <- renderInfoBox({
    infoBox(
      "Weekday/40-60", sum(data()[which(data()$TimeBand=="Weekday29"),]$NetPrice), icon = icon("th-list", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$weekday29elastic <- renderInfoBox({
    infoBox(
      "Weekday/40-60/elastic", (sum(data()[which(data()$TimeBand=="Weekday29"),]$NetPrice)*input$band3)/(input$sensitivity3), icon = icon("indent-left", lib = "glyphicon"),
      color = "green"
    )
  })
  output$weekday29diff <- renderInfoBox({
    infoBox(
      "Weekday/40-60/diff", ((sum(data()[which(data()$TimeBand=="Weekday29"),]$NetPrice)*input$band3/(input$sensitivity3))-sum(data()[which(data()$TimeBand=="Weekday29"),]$NetPrice)), icon = icon("usd", lib = "glyphicon"),
      color = "purple"
    )
  })
  output$weekday36 <- renderInfoBox({
    infoBox(
      "Weekday/60-80", sum(data()[which(data()$TimeBand=="Weekday36"),]$NetPrice), icon = icon("th-list", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$weekday36elastic <- renderInfoBox({
    infoBox(
      "Weekday/60-80", (sum(data()[which(data()$TimeBand=="Weekday36"),]$NetPrice)*input$band4)/(input$sensitivity4), icon = icon("indent-left", lib = "glyphicon"),
      color = "green"
    )
  })
  output$weekday36diff <- renderInfoBox({
    infoBox(
      "Weekday/60-80/elastic", ((sum(data()[which(data()$TimeBand=="Weekday36"),]$NetPrice)*input$band4/(input$sensitivity4))-sum(data()[which(data()$TimeBand=="Weekday36"),]$NetPrice)), icon = icon("usd", lib = "glyphicon"),
      color = "purple"
    )
  })
  output$weekday41 <- renderInfoBox({
    infoBox(
      "Weekday/above80", sum(data()[which(data()$TimeBand=="Weekday41"),]$NetPrice), icon = icon("th-list", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$weekday41elastic <- renderInfoBox({
    infoBox(
      "Weekday/above80/elastic", (sum(data()[which(data()$TimeBand=="Weekday41"),]$NetPrice)*input$band5)/(input$sensitivity5), icon = icon("indent-left", lib = "glyphicon"),
      color = "green"
    )
  })
  output$weekday41diff <- renderInfoBox({
    infoBox(
      "Weekday/above80/diff", ((sum(data()[which(data()$TimeBand=="Weekday41"),]$NetPrice)*input$band5/(input$sensitivity5))-sum(data()[which(data()$TimeBand=="Weekday41"),]$NetPrice)), icon = icon("usd", lib = "glyphicon"),
      color = "purple"
    )
  })
  output$weeknight13 <- renderInfoBox({
    infoBox(
      "Weeknight/4pm-6am", sum(data()[which(data()$TimeBand=="Weeknight13"),]$NetPrice), icon = icon("th-list", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$weeknight13elastic <- renderInfoBox({
    infoBox(
      "Weeknight/4pm-6am/elastic", (sum(data()[which(data()$TimeBand=="Weeknight13"),]$NetPrice)*input$band6)/(input$sensitivity6), icon = icon("indent-left", lib = "glyphicon"),
      color = "green"
    )
  })
  output$weeknight13diff <- renderInfoBox({
    infoBox(
      "Weeknight/4pm-6am/diff", ((sum(data()[which(data()$TimeBand=="Weeknight13"),]$NetPrice)*input$band6/(input$sensitivity6))-sum(data()[which(data()$TimeBand=="Weeknight13"),]$NetPrice)), icon = icon("usd", lib = "glyphicon"),
      color = "purple"
    )
  })
  output$weekend <- renderInfoBox({
    infoBox(
      "Weekend", sum(data()[which(data()$TimeBand=="Weekend"),]$NetPrice), icon = icon("th-list", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$weekendelastic <- renderInfoBox({
    infoBox(
      "Weekend/elastic", (sum(data()[which(data()$TimeBand=="Weekend"),]$NetPrice)*input$band7)/(input$sensitivity7), icon = icon("indent-left", lib = "glyphicon"),
      color = "green"
    )
  })
  output$weekenddiff <- renderInfoBox({
    infoBox(
      "Weekend/diff", ((sum(data()[which(data()$TimeBand=="Weekend"),]$NetPrice)*input$band7/(input$sensitivity7))-sum(data()[which(data()$TimeBand=="Weekend"),]$NetPrice)), icon = icon("usd", lib = "glyphicon"),
      color = "purple"
    )
  })
  output$total <- renderInfoBox({
    infoBox(
      "totalRevenue", sum(data()$NetPrice), icon = icon("th-list", lib = "glyphicon"),
      color = "yellow", fill=TRUE
    )
  })
  output$totalelastic <- renderInfoBox({
    infoBox(
      "totalRevenue/elastic",(sum(data()[which(data()$TimeBand=="Weekend"),]$NetPrice)*input$band7/(input$sensitivity7)+
                                             sum(data()[which(data()$TimeBand=="Weeknight13"),]$NetPrice)*input$band6/(input$sensitivity6)+
                                sum(data()[which(data()$TimeBand=="Weekday41"),]$NetPrice)*input$band5/(input$sensitivity5)+
                                sum(data()[which(data()$TimeBand=="Weekday36"),]$NetPrice)*input$band4/(input$sensitivity4)+
                                sum(data()[which(data()$TimeBand=="Weekday29"),]$NetPrice)*input$band3/(input$sensitivity3)+
                                sum(data()[which(data()$TimeBand=="Weekday19"),]$NetPrice)*input$band2/(input$sensitivity2)+
                                sum(data()[which(data()$TimeBand=="Weekday9"),]$NetPrice)*input$band1/(input$sensitivity1))
                                , icon = icon("indent-left", lib = "glyphicon"),
                                color = "green", fill=TRUE
    )
  })
  output$totaldiff <- renderInfoBox({
    infoBox(
      "totalRevenue/diff",((sum(data()[which(data()$TimeBand=="Weekend"),]$NetPrice)*input$band7/(input$sensitivity7)+
                              sum(data()[which(data()$TimeBand=="Weeknight13"),]$NetPrice)*input$band6/(input$sensitivity6)+
                              sum(data()[which(data()$TimeBand=="Weekday41"),]$NetPrice)*input$band5/(input$sensitivity5)+
                              sum(data()[which(data()$TimeBand=="Weekday36"),]$NetPrice)*input$band4/(input$sensitivity4)+
                              sum(data()[which(data()$TimeBand=="Weekday29"),]$NetPrice)*input$band3/(input$sensitivity3)+
                              sum(data()[which(data()$TimeBand=="Weekday19"),]$NetPrice)*input$band2/(input$sensitivity2)+
                              sum(data()[which(data()$TimeBand=="Weekday9"),]$NetPrice)*input$band1/(input$sensitivity1))-sum(data()$NetPrice))
      , icon = icon("usd", lib = "glyphicon"),
      color = "purple", fill=TRUE
    )
  })
  
 
  
  
}
shinyApp(ui, server)