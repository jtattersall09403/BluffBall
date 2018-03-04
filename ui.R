# ------------- BluffBall - use bookie's odds to optimise fantasy premier league performance ----------------

library(shiny)
library(shinyLP)
library(shinydashboard)
library(shinythemes)
library(DT)
library(plotly)

# Set gameweek
gw <- 28

# Define UI for application
shinyUI(dashboardPage(skin = 'green',
  
  # Set up dashboard layout
  dashboardHeader(title = 'BluffBall'),
  
  # Side bar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Point predictions", tabName = "odds", icon = icon("dashboard")),
      menuItem("Rate my team", tabName = "team", icon = icon("th")),
      menuItem("Recommended transfers", tabName = "trans", icon = icon("cog")),
      menuItem("Dreamteam", tabName = "dt", icon = icon("far fa-star"))
    )
  ),
  
  # Dashboard content
  dashboardBody(
    tags$head(HTML(includeText('google-analytics.js'))),
    tabItems(
      # Landing page
      tabItem("home",
               
               jumbotron("BluffBall",
                         em("Use bookies' odds to optimise fantasy premier league performance"),
                         button = FALSE
                         ),
               br(),
               fluidRow(
                 box(width = 12,
                     title = 'Welcome',
                     status = 'success',
                     solidHeader = T,
                     HTML(paste0(h3('The bookies make a living out of accurately predicting the future. ',
                        strong('BluffBall'), ' lets you turn their hard work into fantasy premier league points. '))),
                     br(),
                     p('By using a combination of bookies odds and statistical modelling, we work out ',
                       'which players are most likely to score, get assists, keep clean sheets, and play more than 60 minutes in the upcoming gameweek. ',
                       'We then use this information to tell you which team to put out, and which transfers are going to improve your squad the most.'),
                     br(),
                     HTML('To get started, check out the <b>point predictions</b> tab, or head to <b>Rate my team</b> to import your team and optimise your squad.')
                     )
                 
                )
              ),
      tabItem(tabName = 'odds',
              h2(paste('Fantasy premier league point predictions: Gameweek', gw)),
              p('The tables below use the latest odds to show which players are ',
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
              ),
              fluidRow(
                box(width = 12,
                    title = 'All point predictions',
                    status = 'success',
                    solidHeader = T,
                    DT::dataTableOutput('point_projections'))
              )
      ),
      
      tabItem(tabName = 'team',
              h2(paste('Rate my team: Gameweek', gw)),
              p(HTML(paste('1. Go to <a href="fantasy.premierleague.com"> fantasy.premierleague.com</a>'))),
              p(HTML(paste('2. Click on the', strong('points'), 'tab, copy the URL and paste it into the box below'))),
              p('3. Click "import" to import your team and calculate how many points you', "'re likely to get this gameweek."),
              fluidRow(
                box(width = 12,
                    textInput('teamid', label = 'Enter team ID', value = 'fantasy.premierleague.com/a/team/4880044/event/28'),
                    textInput('gw', label = 'Gameweek', value = '28'),
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
              h2(paste('Recommended transfers: Gameweek', gw)),
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
              ),
              fluidRow(
                box(width = 12,
                    status = 'success',
                    title = 'Make transfers',
                    solidHeader = T,
                    p('You can experiment with different transfers here. Select players to transfer out in the table on the left, and players to transfer in using the table on the right. Then click the transfers button below.'),
                    h3('Bank:', textOutput('bankval')),
                    box(width = 6, title = 'Transfers out', DTOutput('currentsquad')),
                    box(width = 6, title = 'Transfers in', DTOutput('playerpool')),
                    actionButton('transfers', 'Make transfers', class = "btn btn-primary")
                )
              ),
              fluidRow(
                box(width = 6,
                    status = 'success',
                    title = 'New team',
                    solidheader = TRUE,
                    plotOutput('newteamplot')),
                box(width = 6,
                    title = 'New team stats',
                    status = 'success',
                    box(width=6,
                        title = 'Total cost',
                        status = 'success',
                        solidHeader = TRUE,
                        h1(textOutput('newcost'))),
                    box(width=6,
                        title = 'Expected points',
                        status = 'success',
                        solidHeader = TRUE,
                        h1(textOutput('newxp')))
                )
              )
      ),
      tabItem(tabName = 'dt',
              h2(paste("BluffBall's affordable dreamteam for gameweek", gw)),
              em('"Did you see that ludicrous display last night?"'),
              p(),
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
