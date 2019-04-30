#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
#setwd("~/svn/zoo/Stage")

library(recommenderlab)
library(stringr)
library(arules)

generate_row_filter <- function (database, interesting){
  if (length(interesting) == 0 ){
    return(rep(T, nrow(database)))
  }
  interesting_colnames <- str_replace(interesting, "=[0-9]*", "")
  interesting_values <- str_replace(interesting, "[a-zA-Z]*=", "")
  okay <- rep(F, nrow(database))
  for (i in 1:length(interesting_values)) {
    okay[database[[interesting_colnames[i]]] == interesting_values[i]] = T
  }
  
  return(okay)
}
generate_pattern_filter <- function (database, interesting){
  if (length(interesting) == 0 ){
    return(rep(T, nrow(database)))
  }
  return(paste(str_replace(str_replace(interesting, "^", "`"), "$", "`"), collapse = " | "))
}


database <- readRDS("zoodb.Rds")
pattern_database <- readRDS("zoodb_patterns.Rds")

library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  analysis_df <- as(database, "transactions")
  
  r <- as(analysis_df, "binaryRatingMatrix")
  rec <- Recommender(r, method="UBCF")
  
  ##
  ## USER PREFERENCES
  ##
  
  #as(predict(rec, user), "list")
  
  output$rawRatings <- renderPlot({ image(r, main="Raw Ratings") })
  output$parameters <- renderDataTable( {prefs} )
  output$data <- renderDataTable({
    df<-database %>% filter(generate_row_filter(database, input$interesting_vars))
    datatable(df, filter="top", extensions = c('Buttons'), options = list(
      order=list(c(0, 'asc')),
      dom = 'Bfrtip',
      buttons = list('copy', 
                     'csv', 
                     'excel', 
                     'pdf', 
                     'print', 
                     'colvis')
      ,
      scrollX = TRUE))
  })
  
  output$patterns <- renderDataTable({
    df<-pattern_database %>% filter_(generate_pattern_filter(pattern_database, input$interesting_vars))
    datatable(df, filter="top", extensions = c('Buttons'), options = list(
      order=list(c(0, 'asc')),
      dom = 'Bfrtip',
      buttons = list('copy', 
                     'csv', 
                     'excel', 
                     'pdf', 
                     'print', 
                     'colvis')
      ,
      scrollX = TRUE))
  })
  
  output$recommendations <- renderUI({
    if (length(input$interesting_vars) == 0){
      result <- "Please select something you find interesting first."
    }else if(length(input$interesting_vars) == length(rec@model$data@data@itemInfo$labels)){
      result<- "Well, everything interests you already!"
    }else{
      prefs <- rec@model$data@data@itemInfo$labels %in% input$interesting_vars
      userPrefs <- t(as.matrix(prefs, ncol=length(rec@model$data@data@itemInfo$labels)))
      colnames(userPrefs)<- rec@model$data@data@itemInfo$labels
      user <- as(userPrefs, "binaryRatingMatrix")
      
      result <- paste(unlist(as(predict(rec, user), "list")),collapse=", ")
      result <- paste("You might find those interesting too:", result)
    }
    print(result)
  })
})
