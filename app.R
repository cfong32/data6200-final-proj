library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
source('preprocess.R')

#####
ui <- dashboardPage(
  # Heading
  dashboardHeader(title='Life Expectancy'),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem('Introduction' , tabName='Introduction', icon=icon('circle-info')),
      menuItem('Ranking'      , tabName='Ranking'     , icon=icon('list-ol')),
      menuItem('Trends'       , tabName='Trends'      , icon=icon('chart-line')),
      menuItem('Economic'     , tabName='Economic'    , icon=icon('bank')),
      menuItem('Mortality'    , tabName='Mortality'   , icon=icon('skull')),
      menuItem('Vaccination'  , tabName='Vaccination' , icon=icon('syringe')),
      menuItem('Comparing'    , tabName='Comparing'   , icon=icon('shuffle'))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 0
      tabItem(tabName='Introduction',
              fluidRow(box(width=12,
                           title=h1('Dashboard on Life Expectancy'),
                           tags$p(h4('Author: Chung Yan FONG'),
                                  h4('Date: 2022-Dec-12'))),
                       box(width=12,
                           title=h1('What is Life Expectancy?'),
                           tags$p(text0a)),
                       box(width=12,
                           title=p(h1('The Dataset'),
                                   h4('183 Countries, 2000-2015')),
                           tags$p(text0b))
                       )
              ),
      # Tab 1
      tabItem(tabName='Ranking',
              fluidRow(box(width=12,
                           selectInput("ctl1a","Color by:",
                                       choices=c('Country', 'Continent', 'Status'),
                                       selected='Country')
                           ),
                       box(width=12,
                           title=h1('In Different Countries'),
                           plotlyOutput('plot1', height=400),
                           ),
                       box(title=h2('Top 10'),
                           tableOutput('table2')
                           ),
                       box(title=h2('Bottom 10'),
                           tableOutput('table3')
                           )
                       )
              ),
      
      # Tab 6
      tabItem(tabName='Trends',
              fluidRow(box(width=12,
                           selectInput("ctl6a","Group by:",
                                       choices=c('Continent', 'Status'),
                                       selected='Status')
                           ),
                       box(width=12,
                           title=h1('Trends (2000-2015)'),
                           plotlyOutput('plot6ab', height=400)
                           ),
                       )
              ),
      
      # Tab 2
      tabItem(tabName='Economic',
              fluidRow(box(width=12,
                           selectInput("ctl2a","Group by:",
                                       choices=c('Continent', 'Status'),
                                       selected='Status')
                           ),
                       box(width=12,
                           title=h1('Life expectancy vs GDP'),
                           plotlyOutput('plot2ab', height=400)
                           ),
                       box(width=12,
                           title=h1('Health expenditure per capita'),
                           plotlyOutput('plot2cd', height=400)
                           )
                       )
              ),
      
      # Tab 3
      tabItem(tabName='Vaccination',
              p(h1('Immunization by Vaccination'),
                h4('Coverage among 1-year-olds (%)')),
              fluidRow(box(title=h2('Hepatitis B'),
                           plotlyOutput('plot3a', height=300)
                           ),
                       box(title=h2('Polio'),
                           plotlyOutput('plot3b', height=300)
                           ),
                       box(title=h2('Diphtheria'),
                           plotlyOutput('plot3c', height=300)
                           ),
                       box(title=h2('Average of above'),
                           plotlyOutput('plot3d', height=300)
                           )
                      )
              ),

      # Tab 4
      tabItem(tabName='Mortality',
              fluidRow(box(width=12,
                           selectInput("ctl4a","Group by:",
                                       choices=c('Continent', 'Status'),
                                       selected='Status')
                           ),
                       box(width=12,
                           title=p(h1('Adult Mortality'),
                                   h4('Per 1000 population, aged 15-60')),
                           plotlyOutput('plot4a', height=400)
                           ),
                       box(width=12,
                           title=p(h1('Child Deaths'),
                                   h4('Per 1000 population, aged under 5')),
                           plotlyOutput('plot4b', height=400)
                           ),
                       box(width=12,
                           title=p(h1('Child HIV/AIDS Deaths'),
                                   h4('Per 1000 live births')),
                           plotlyOutput('plot4c', height=400)
                           )
                       )
              ),
      
      # Tab 5
      tabItem(tabName='Comparing',
              p(h1('Country-to-Country Comparison'),
                h3('Trends in 2000-2015')),
              fluidRow(box(width=12,
                           selectInput("ctl5a", "Select some countries:",
                                       choices=countries,
                                       selected=c('Canada', 'South Africa', 'Russia', 'India'),
                                       multiple=TRUE)
                           ),
                       box(width=12,
                           title=h1('Ranking'),
                           tableOutput('table5a')
                           ),
                       box(width=12,
                           title=p(h1('Immunization'),
                                   h4('Average immunization of Hepatitis B, Polio and Diphtheria (% among 1-year-olds)')
                                   ),
                           plotlyOutput('plot5b', height=400)
                           ),
                       box(width=12,
                           title=p(h1('Adult Mortality'),
                                   h4('Aged 15-60, per 1000 population')),
                           plotlyOutput('plot5c', height=400)
                           ),
                       box(width=12,
                           title=p(h1('Child Deaths'),
                                   h4('Aged under 5, per 1000 population')),
                           plotlyOutput('plot5d', height=400)
                           ),
                       box(width=12,
                           title=p(h1('Child HIV/AIDS Deaths'),
                                   h4('Per 1000 live births')),
                           plotlyOutput('plot5e', height=400)
                           ),
                       box(width=11,
                           title=p(h1('Gross Domestic Product'),
                                   h4('Per capita, in USD')),
                           plotlyOutput('plot5f', height=400)
                           ),
                       box(width=12,
                           title=p(h1('Expenditure on Health'),
                                   h4('Per capita, in USD')),
                           plotlyOutput('plot5g', height=400)
                           ),
                       )
              )
      )
    )
)

#####
server <- function(input, output) {
  show_plot1 <- reactive({switch(input$ctl1a, 'Country'=plot1a, 'Continent'=plot1b, 'Status'=plot1c)})
  output$plot1 <- renderPlotly({show_plot1()})
  output$table2 <- renderTable(df_top)
  output$table3 <- renderTable(df_bottom)
  
  show_plot2ab <- reactive({switch(input$ctl2a, 'Status'=plot2a, 'Continent'=plot2b)})
  show_plot2cd <- reactive({switch(input$ctl2a, 'Status'=plot2c, 'Continent'=plot2d)})
  output$plot2ab <- renderPlotly({show_plot2ab()})
  output$plot2cd <- renderPlotly({show_plot2cd()})
  
  output$plot3a <- renderPlotly({plot3a})
  output$plot3b <- renderPlotly({plot3b})
  output$plot3c <- renderPlotly({plot3c})
  output$plot3d <- renderPlotly({plot3d})
  
  show_plot4a <- reactive({switch(input$ctl4a,
                                  'Status'   =plot4x(Adult_mortality, Status),
                                  'Continent'=plot4x(Adult_mortality, Continent))})
  show_plot4b <- reactive({switch(input$ctl4a,
                                  'Status'   =plot4x(Child_deaths, Status),
                                  'Continent'=plot4x(Child_deaths, Continent))})
  show_plot4c <- reactive({switch(input$ctl4a,
                                  'Status'   =plot4x(Child_HIV_deaths, Status),
                                  'Continent'=plot4x(Child_HIV_deaths, Continent))})
  
  output$plot4a <- renderPlotly({show_plot4a()})
  output$plot4b <- renderPlotly({show_plot4b()})
  output$plot4c <- renderPlotly({show_plot4c()})
  
  show_table5a <- reactive({table5a(input$ctl5a)})
  show_plot5b <- reactive({plot5x(input$ctl5a, Avg_immunization)})
  show_plot5c <- reactive({plot5xlogy(input$ctl5a, Adult_mortality)})
  show_plot5d <- reactive({plot5xlogy(input$ctl5a, Child_deaths)})
  show_plot5e <- reactive({plot5xlogy(input$ctl5a, Child_HIV_deaths)})
  show_plot5f <- reactive({plot5xlogy(input$ctl5a, GDP)})
  show_plot5g <- reactive({plot5xlogy(input$ctl5a, Health_GDP)})

  output$table5a <- renderTable(show_table5a())
  output$plot5b <- renderPlotly({show_plot5b()})
  output$plot5c <- renderPlotly({show_plot5c()})
  output$plot5d <- renderPlotly({show_plot5d()})
  output$plot5e <- renderPlotly({show_plot5e()})
  output$plot5f <- renderPlotly({show_plot5f()})
  output$plot5g <- renderPlotly({show_plot5g()})

  show_plot6ab <- reactive({switch(input$ctl6a, 'Status'=plot6a, 'Continent'=plot6b)})
  output$plot6ab <- renderPlotly({show_plot6ab()})
  
}

#####
shinyApp(ui, server)
