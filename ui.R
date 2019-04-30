#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

library(DT)
#setwd("~/svn/zoo/Stage")

analysis_df <- readRDS("analysis_df.Rds")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Recommender system"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout( position="right",
    sidebarPanel(
      checkboxGroupInput('interesting_vars', 'Interesting parameters:', analysis_df@itemInfo$labels, rep(F, length(analysis_df@itemInfo$labels)) )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #plotOutput("rawRatings"),
      uiOutput("recommendations"),
      h2("Concerned patterns"),
      DTOutput("patterns"),
      h2("Concerned data"),
      DTOutput("data")
    )
  )
))
