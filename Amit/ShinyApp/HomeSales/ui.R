#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyApp(onStart=NULL, server="server.R",
  # Define UI for application that draws a histogram
  shinyUI(fluidPage(
  
    # Application title
    titlePanel("Home Sales Data"),
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
       sliderInput("seed",
                   "Regression seed:",
                   min = 1,
                   max = 1000,
                   value = 800),
       textInput("splitPercent", label="Train/Test Split %:", value = "0.80", width = NULL, placeholder = NULL),
       textInput("signifCode", label="Significance level for Regression Coefficients:", value = "0.05", width = NULL, placeholder = NULL),
       
       selectInput("select", label = h4("Significance level for Regression Coefficients:"), 
                   choices = list("0" = 1, "0.001" = 2,
                                  "0.05" = 3, "0.1" = 4,"1" = 5), selected = 3)
    ),
    
    mainPanel(
       h3("Current List Price vs Scaled List  Price"),
       plotOutput(("scatter")),
      
       h3("Linear Regression results"),
       tableOutput("prediction"),
       h3("Residual Plots"),
       plotOutput("displayPlot"),
       h3("Regression Summary"),
       textOutput("regressionSummary"),
       tableOutput("regressionTable"),
       # fluidRow(
       #   DT::dataTableOutput("regressionSummary")
       # ),
       h3("Predictions: List Price"),
       fluidRow(
         DT::dataTableOutput("table")
       )
    )
  )
))
)