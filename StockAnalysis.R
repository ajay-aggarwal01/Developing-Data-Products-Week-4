


###http://download.macrotrends.net/assets/php/stock_data_export.php?t=ORCL


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Read Data
setwd("D:/Work/mystuff/Education/DataScience/Developing Data Products")
msft <- read.csv("MSFT.csv", stringsAsFactors = FALSE)
orcl <- read.csv("ORCL.csv", stringsAsFactors = FALSE)
crm <- read.csv("CRM.csv", stringsAsFactors = FALSE)

msft$Year <- strftime(as.Date(msft$date, "%m/%d/%Y"), "%Y")
# msft$month <- strftime(as.Date(msft$date, "%m/%d/%Y"),"%b")
msft$month <- strftime(as.Date(msft$date, "%m/%d/%Y"),"%m")

orcl$Year <- strftime(as.Date(orcl$date, "%m/%d/%Y"), "%Y")
orcl$month <- strftime(as.Date(orcl$date, "%m/%d/%Y"),"%m")

crm$Year <- strftime(as.Date(crm$date, "%m/%d/%Y"), "%Y")
crm$month <- strftime(as.Date(crm$date, "%m/%d/%Y"),"%m")


# Define UI for application that draws a plot
ui <- fluidPage(
  
  # Application title
  titlePanel(title = h3("Oracle, Salesforce and Microsoft Stock Pattern Analysis", align="center")),
  br(),   br(),
  sidebarLayout(
    sidebarPanel(
      #------------------------------------------------------------------
      # Add radio button to choice for north pole or south pole data
      radioButtons("poleInput", 
                   label = "Select Data: ",
                   choices = list("Microsoft" = 'msft', "Oracle" = 'orcl', "Salesforce" = 'crm'),
                   selected = 'msft'),
      br(),   br(),
      #------------------------------------------------------------------
      # Add Variable for Year Selection
      sliderInput("YearRange", "Select Year Range : ", min=2005, max=2019, value=c(2005, 2019), step=1      ),
      
      br(),   br(),
      #------------------------------------------------------------------
      # Add Variables selection  open, high, low, close, volume etc : 
      selectInput("var", "Select Variable from Dataset", 
                  choices=c("open"=2, "high"= 3 , "low"=4, "close"=5, 
                            "volume"=6, "Year"=7, "Month"=8),
                              multiple=TRUE, selected = 2),
      
      br(),   br()
      #------------------------------------------------------------------
      # Change background color for body
      #tags$style("body{background-color:lightyellow; color:brown}")
    ),
    
    mainPanel(
      #------------------------------------------------------------------
      # Create tab panes
      tabsetPanel(type="tab",
                  tabPanel("Summary",verbatimTextOutput("sumry")),
                  tabPanel("Structure", verbatimTextOutput("struct")),
                  tabPanel("Data", tableOutput("displayData")),
                  tabPanel("Plot", plotOutput("mygraph"))
      )
      
      #------------------------------------------------------------------
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  cols <- reactive({
    as.numeric(c(input$var))
    
  })
  mylabel <- reactive({
    if(input$poleInput=='msft'){
      lable <- "Plot for Microsoft Data"
    }
    if(input$poleInput=='orcl'){
      lable <- "Plot for Oracle  Data"
    }
    if(input$poleInput=='crm'){
      lable <- "Plot for Salesforce Data"
    }
    lable
  })
  
  
  myFinalData <- reactive({
    #------------------------------------------------------------------
    # Select data according to selection of ratdio button
      mydata <- msft
      
    if(input$poleInput=='msft'){
      mydata <- msft
      
    }
    
    if(input$poleInput=='orcl'){
      mydata <- orcl
    }
    
    if(input$poleInput=='crm'){
      mydata <- crm
    }
    #------------------------------------------------------------------
    # Get data rows for selected year
    mydata1 <- mydata[mydata$Year >= input$YearRange[1], ] # From Year
    mydata1 <- mydata1[mydata1$Year <= input$YearRange[2], ] # To Year
    #------------------------------------------------------------------
    # Get Data for selected data sets as variable
    
    mydata2<- mydata1[, c(7, sort(cols()))]    
    ##mydata2 <- mydata1[which(mydata1$month %in%  sort(cols())),]
    
    #------------------------------------------------------------------
    # Get data rows for selected year
    data.frame(mydata2)
    #------------------------------------------------------------------
    
  })
  
  # Prepare "Data tab"
  output$displayData <- renderTable({
    myFinalData()
  })
  
  # Prepare Structure Tab
  renderstr <- reactive({ str(myFinalData())})
  
  output$struct <- renderPrint({
    renderstr()
  })
  
  # Prepare Summary Tab
  rendersumry <- reactive({ summary(myFinalData())})
  
  output$sumry <- renderPrint({
    rendersumry()
  })
  
  # Prepare Plot Tab
  output$mygraph <- renderPlot({
    plotdata <- myFinalData()
    
    plot(plotdata, col=c(2,3,4,5,6,7,8), main=mylabel())

  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

