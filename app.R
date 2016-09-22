## app.R ##
library(shiny)
library(shinydashboard)
library(insuranceData)
library(earth)
library(plotmo)
library(randomForest)
ui <- dashboardPage(
  dashboardHeader(title = "Insurance Modelling"),
  dashboardSidebar(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    sidebarMenu(
      menuItem("GLM", tabName = "GLM", icon = icon("dashboard")),
      menuItem("MARS", tabName = "MARS", icon = icon("th")),
      menuItem("Random Forest", tabName = "rf", icon = icon("th"))
    ),
    numericInput("degree", 
                 label = ("Degree of Interaction"), 
                 value = 1),
    sliderInput("nfold", label = ("Number of folds for Cross-validation"),
                min = 0, max = 20, value = 1),
    sliderInput("prune", label = ("Number of Terms"),
                min = 0, max = 100, value = 20),
    sliderInput("tree", label = ("Number of Trees"),
                min = 10, max = 1000, value = 100)
    ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "GLM",
              fluidRow(
                tabBox(
                  title = "GLM Model",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("Dataset", dataTableOutput('dataset')),
                  tabPanel("Summary", verbatimTextOutput('glmsummary')),
                  tabPanel("Plot", plotOutput('glmplot'))
                )
              )
      ),
      
      
      # Second tab content
      tabItem(tabName = "MARS",
              fluidRow(
                tabBox(
                  title = "MARS Model",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("Summary", verbatimTextOutput('marssummary')),
                  tabPanel("Plots", plotOutput('marsplot')),
                  tabPanel("PlotMo", plotOutput('marsplotmo')),
                  tabPanel("Cross-Validation", plotOutput('marsplotcross')),
                  tabPanel("Variable Importance", verbatimTextOutput('marsevimp'))
                )
              )
      ),
      # third tab content
      tabItem(tabName = "rf",
              fluidRow(
                tabBox(
                  title = "RandomForest Model",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("RF summary", verbatimTextOutput('rf')),
                  tabPanel("RF plot", plotOutput('rfplot')),
                  tabPanel("Variable Importance", plotOutput('rfimportance'))
                )
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
output$dataset<-renderDataTable(data())
output$glmsummary<-renderPrint(summary(glm(LOSS~MARITAL+SEATBELT+CLMAGE,data(),family=gaussian(link = "identity"))))
output$glmplot<-renderPlot(plot(glm(LOSS~MARITAL+SEATBELT+CLMAGE,data(),family=gaussian(link = "identity"))))

#MARSoutputs
output$marssummary<-renderPrint(summary(earth(LOSS~.-CASENUM-X, data(),degree=input$degree,nprune=input$prune,nfold=input$nfold,ncross=1,keepxy=T)))
output$marsplot<-renderPlot(plot(earth(LOSS~.-CASENUM-X, data(),degree=input$degree,nprune=input$prune,nfold=input$nfold,ncross=1,keepxy=T)))
output$marsplotmo<-renderPlot(plotmo(earth(LOSS~.-CASENUM-X, data(),degree=input$degree,nprune=input$prune,nfold=input$nfold,ncross=1,keepxy=T)))
output$marsplotcross<-renderPlot(plot(earth(LOSS~.-CASENUM-X, data(),degree=input$degree,nprune=input$prune,nfold=input$nfold,ncross=1,keepxy=T),which=1,col.rsq=0))
output$marsevimp<-renderPrint(evimp(earth(LOSS~.-CASENUM-X, data(),degree=input$degree,nprune=input$prune,nfold=input$nfold,ncross=1,keepxy=T)))

#RandomForestoutputs
output$rf<-renderPrint(randomForest(LOSS~.-CASENUM-X, data(),ntree=input$tree,importance=TRUE))
output$rfplot<-renderPlot(plot(randomForest(LOSS~.-CASENUM-X, data(),ntree=input$tree,importance=TRUE)))
output$rfimportance<-renderPlot(varImpPlot(randomForest(LOSS~.-CASENUM-X, data(),ntree=input$tree,importance=TRUE)))

}
shinyApp(ui, server)