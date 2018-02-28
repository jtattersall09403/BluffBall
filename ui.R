# ------------- BluffBall - use bookie's odds to optimise fantasy premier league performance ----------------

library(shiny)
library(shinyLP)
library(shinydashboard)
library(shinythemes)
library(DT)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin = 'green',
  
  # Set up dashboard layout
  dashboardHeader(title = 'BluffBall'),
  
  # Side bar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Latest odds", tabName = "odds", icon = icon("dashboard")),
      menuItem("Rate my team", tabName = "team", icon = icon("th")),
      menuItem("Recommended transfers", tabName = "trans", icon = icon("cog")),
      menuItem("Dreamteam", tabName = "dt", icon = icon("far fa-star"))
    )
  ),
  
  # Dashboard content
  dashboardBody(
    tabItems(
      tabItem(tabName = 'odds',
              h2('Fantasy premier league odds'),
              p('The tables below use the latest odds from William Hill to show which players are ',
                ' most likely to score and which teams are most likely to get a clean sheet in the next game week.'),
              fluidRow(
                box(width = 6,
                    title = 'Goalscorer probabilities',
                    DT::dataTableOutput('goalscorer_tab')
                ),
                box(width = 6,
                    title = 'Clean sheet probabilities',
                    DT::dataTableOutput('cs_tab')
                )
              )       
      ),
      
      tabItem(tabName = 'team',
              h2('Rate my team'),
              p('Enter your team ID below and click "import" to import your fantasy team',
                ' and display the best starting 11 for your current squad.',
                ' You can get your team ID ',
                'from the middle of the URL in your browser on the "points" page of the FPL website. '),
              fluidRow(
                box(width = 12,
                    textInput('teamid', label = 'Enter team ID', value = '4880044'),
                    textInput('gw', label = 'Gameweek', value = '27'),
                    actionButton('import_team', label = 'Import'))
              ),
              fluidRow(
                box(width = 6,
                    title = 'Current team expected points',
                    solidHeader = T,
                    status = 'success',
                    h1(textOutput("totxp_1")))
              ),
              fluidRow(
                box(width = 6,
                    status = 'primary',
                    title = 'Current team',
                    DT::dataTableOutput('myteam_tab')),
                box(width = 6,
                    status = 'success',
                    title = 'Current team',
                    solidHeader = T,
                    plotOutput('current_team'))
                
              ),
              fluidRow(
                box(width = 6,
                    title = 'Best 11 expected points',
                    solidHeader = T,
                    status = 'success',
                    h1(textOutput("totxp_2")))
              ),
              fluidRow(
                box(width = 6,
                    status = 'primary',
                    title = 'Best starting 11',
                    DT::dataTableOutput('myteam3_tab')),
                box(width = 6,
                    status = 'success',
                    title = 'Best 11',
                    solidHeader = T,
                    plotOutput('first_team'))
              )
     
      ),
      
      tabItem(tabName = 'trans',
              h2('Recommended transfers'),
              p("Once you've imported your team on the 'Rate my team' tab, the tables below will show you ",
                "the best transfer(s) you can make to optimise your team. Enter the amount you currently have in the ",
                "bank, as well as the number of free transfers you have, and click 'Optimise'."),
              fluidRow(
                box(width = 12,
                    numericInput('bank', label = 'Amount in bank', value = 0.3),
                    numericInput('ft', label = 'Free transfers', value = 1),
                    actionButton('optimise', label = 'Optimise'))
              ),
              
              fluidRow(
                box(width = 6,
                    status = 'primary',
                    title = 'Best 3 transfer targets per player',
                    DT::dataTableOutput('single_trans')
                    ),
                box(width = 6,
                    status = 'primary',
                    title = 'Best double transfers',
                    p('Accounting for any transfer penalties'),
                    DT::dataTableOutput('double_trans')
                    )
              )
      ),
      tabItem(tabName = 'dt',
              h2("BluffBall's affordable dreamteam for the next gameweek"),
              fluidRow(
                box(width = 6,
                  title = 'Expected points',
                  solidHeader = T,
                  status = 'success',
                  h1(textOutput('dtxp'))),
              box(width = 6,
                  title = 'Total cost',
                  solidHeader = T,
                  status = 'success',
                  h1(textOutput('dtcost')))
              ),
              fluidRow(
                box(width = 6,
                    title = 'Dreamteam',
                    status = 'primary',
                    DT::dataTableOutput('dreamteam')),
                box(width = 6,
                    title = '',
                    status = 'primary',
                    plotOutput('dreamteam_vis'))
              )
    )

    
  )

  
)))
