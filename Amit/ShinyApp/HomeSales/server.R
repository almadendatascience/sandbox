#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DMwR)   #scale/unscale
library(ggplot2)#graphics
library(caret)  #test/train
library(DT)     #datatable

data.cleaned = read.csv("data/AllData_Cleaned.csv")
data.numeric = data.cleaned[,40:67]
data.numeric.scaled = data.frame(scale(data.numeric, center = TRUE, scale = TRUE))

# Define server logic
shinyServer(function(input, output) {

#  runRegression <- reactive(function(){
  runRegression <- function()({
    #set seed
    set.seed(input$seed)
    split=0.80
    trainIndex <- createDataPartition(data.numeric.scaled$Curr.List.Price, p=split, list=FALSE)
    
    data_train <- data.numeric.scaled[ trainIndex,]
    data_test  <- data.numeric.scaled[-trainIndex,]
    
    # train the linear regression model
    model <- lm(Curr.List.Price~., data=data_train)
    model
  })

  getTestData <-function()({
    set.seed(input$seed)
    split=0.80
    trainIndex <- createDataPartition(data.numeric.scaled$Curr.List.Price, p=split, list=FALSE)
    data_test  <- data.numeric.scaled[-trainIndex,]
    data_test
  })  

  # examine results
  output$displayPlot <- renderPlot({
    mod = runRegression()
        par(mfrow=c(2,2))
    plot(mod)  
  })

  # examine results
  output$scatter <- renderPlot({
    par(mfrow=c(1,4))
    hist(data.numeric$Curr.List.Price)
    plot(1:nrow(data.numeric), data.numeric$Curr.List.Price)
    hist(data.numeric.scaled$Curr.List.Price)
    plot(1:nrow(data.numeric.scaled), data.numeric.scaled$Curr.List.Price)
  })

 output$regressionSummary <- renderText({
    regOutput = c()
    mod = runRegression()
    m = summary(mod)
    regOutput = c("R^2 = ", m$r.squared, " | ", "Adj R^2 = ", m$adj.r.squared)
})
 
 output$regressionTable <- renderTable({
   mod = runRegression()
   m = summary(mod)
   coeffs    = data.frame(m$coefficients[,-c(2:4)])
   #cf = data.frame(coeffs[1,1], coeffs[2,1], coeffs[3,1], coeffs[4,1], coeffs[5,1])
   m
   
 })
  
# Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
        # make predictions
        data_test = getTestData()
        mod = runRegression()
        x_test <- data_test[,1:27]
        y_test <- data_test[,28]
        predictions <- predict(mod, x_test)
        
        #Unscale to arrive at the original test data and predictions for comparison
        data_test_unscaled = lapply(data_test, function(x) {x * sd(data.numeric$Curr.List.Price) + mean(data.numeric$Curr.List.Price)})
        predictions.unscaled = lapply(data.frame(predictions), function(x) {x * sd(data.numeric$Curr.List.Price) + mean(data.numeric$Curr.List.Price)})
    
        combined_prediction = cbind(data_test_unscaled, data.frame(predictions.unscaled))
    combined_prediction[,28:29]
  }))
  
  save_data_flatfile <- function(data) {
    data <- t(data)
    file_name <- "regressionCoefficients.csv"
    
    write.table(x = data, append = TRUE, file = file.path(".", file_name), 
              row.names = FALSE, col.names = FALSE, quote = TRUE)
  }
  
})