# ------------- BluffBall - use bookie's odds to optimise fantasy premier league performance ----------------

library(shiny)
library(shinyLP)
library(shinydashboard)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  actionButton("odds_scrape", "Update odds and fpl data"),
  actionButton("start_model", "Update historic data"),
  dataTableOutput('goalscorer_tab')
  
))
