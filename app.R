library(shinydashboard)
library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)


# shiny-tab-Expenses
# .checkbox, .radio {
#   position: relative;
#   display: block;
#   margin-top: 1px;
#   margin-bottom: 1px;
# }
source('plot-functions.R')

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      sidebarMenu(id='mytabs',
        sidebarMenuOutput("menu")
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      )
      ,tabItems(
        tabItem(tabName = "home_page",h3('home page'))
        ,tabItem(tabName = 'Income'
                 ,fluidRow( column(h3('Income'),width = 2) )
                 ,fluidRow( column(h5('Please enter information about your pre-tax income'),width = 6) )
                 ,tags$br()
                 ,fluidRow( column(uiOutput('incomeTimeSelector'),width = 2), 
                            column(uiOutput('incomeInput'),width = 3), 
                            column(verbatimTextOutput('incomeOther'),width=3)  )
                 ,tags$br()
                 ,fluidRow( column( tags$div(id = 'save_button',actionButton('saveIncome','Save Section')),width = 2)
                            #,textOutput('button')
                            )
                 )
        ,tabItem(tabName = 'Expenses',
                ###-------- dynamic input list --------###  
                fluidRow(column(h3('Category'),width = 3),column(h3('Value'),width = 3))
                
                ,tags$div(class='expense_item',fluidRow(column(h4('Housing'),width = 3) 
                                   ,column(numericInput('housing',label = NULL,value = 1000),width = 3)
                                   ,column(radioButtons("housingTimeSelector",label = NULL,choices = c('Monthly','Annual'),selected = 'Monthly'),width = 1) ))
                
                ,tags$div(id=paste0('myrow',0),class='expense_item',fluidRow(column(textInput(paste0("category_txt", 0),label = NULL,placeholder = 'i.e. Utilities'),width = 3)
                                                        ,column(numericInput(paste0("value_txt", 0),label = NULL,value = 0),width = 3)
                                                        ,column(radioButtons(paste0("timeframe_txt",0),label = NULL,choices = c('Monthly','Annual'),selected = 'Monthly'),width = 1)
                                                        ,column(actionButton(paste0("button", 0),label = NULL,icon = icon("glyphicon glyphicon-minus-sign", lib = "glyphicon")),width = 1)
                ))
                ,tags$div(id = 'placeholder') 
                ,actionButton("add", "Add Line",icon = icon("glyphicon glyphicon-plus-sign", lib = "glyphicon"))
                ,fluidRow(tags$br())
                ,tags$br()
                ,fluidRow( column( tags$div(id = 'save_button',actionButton('saveExpense','Save Section')),width = 2) )
                )
        ,tabItem(tabName = 'Graph',
                 fluidRow(
                  column( box(plotlyOutput('year_plot'),width = 6) ,width = 12)
                 )
                 #textOutput('txt1')
                )
      )
    )
  )


server <- function(input, output, session) {
  
  ##########################################  
  ###----------- sidebar menu -----------###  
  ##########################################
  
  
  output$menu <- renderMenu({
    sidebarMenu(
                menuItem(text = "Income", tabName="Income", icon = icon("usd"),badgeLabel = icon(incomeBadgeIcon(), lib = "glyphicon"), badgeColor = incomeBadgeColor())
                ,menuItem(text = "Expenses", tabName="Expenses", icon = icon("credit-card"),badgeLabel = icon(expenseBadgeIcon(), lib = "glyphicon"), badgeColor = expenseBadgeColor())
                ,menuItem(text = "Graph", tabName="Graph",icon = icon("calendar")) #, icon = icon("credit-card"),badgeLabel = icon(expenseBadgeIcon(), lib = "glyphicon"), badgeColor = expenseBadgeColor())
    )
  })
  isolate({updateTabItems(session, "mytabs", "Income")})
  
  ###----- badge colors and icons -----###
  incomeBadgeColor <- reactive({ if(input$saveIncome>0){'green'}else{'blue'} })
  incomeBadgeIcon <- reactive({ if(input$saveIncome>0){'glyphicon glyphicon-ok'}else{'glyphicon glyphicon-time'} })
  expenseBadgeColor <- reactive({ if(input$saveExpense>0){'green'}else{'blue'} })
  expenseBadgeIcon <- reactive({ if(input$saveExpense>0){'glyphicon glyphicon-ok'}else{'glyphicon glyphicon-time'} })
  
  #output$button <- renderText({ input$saveIncome })
  
  ##########################################  
  ###--------- Income UI inputs ---------###  
  ##########################################
  
  output$incomeTimeSelector <- renderUI({ radioButtons("incomeTimeSelector",label = NULL,choices = c('Annual','Monthly'),selected = 'Annual') })
  output$incomeInput <- renderUI({ textInput("incomeInput", paste(input$incomeTimeSelector,'Income'),value = 0) })
  incomeYearly <- reactive({ if(input$incomeTimeSelector=='Annual'){as.numeric(input$incomeInput)}else{as.numeric(input$incomeInput)*12} })
  incomeMonthly <- reactive({ if(input$incomeTimeSelector=='Monthly'){as.numeric(input$incomeInput)}else{as.numeric(input$incomeInput)/12} })
  output$incomeOther <- renderText({ if(input$incomeTimeSelector=='Annual'){paste('Monthly Income','\n',incomeMonthly())}else{paste('Annual Income','\n',incomeYearly())} })
  
  ##########################################  
  ###-------- dynamic input list --------###  
  ##########################################
    observeEvent(input$add, {
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(id=paste0('myrow',input$add),class='expense_item',fluidRow(column(textInput(paste0("category_txt", input$add),label = NULL),width = 3)
                    ,column(numericInput(paste0("value_txt", input$add),label = NULL,value = 0),width = 3)
                    ,column(radioButtons(paste0("timeframe_txt",input$add),label = NULL,choices = c('Monthly','Annual'),selected = 'Monthly'),width = 1)
                    ,column(actionButton(paste0("button", input$add),label = NULL,icon = icon("glyphicon glyphicon-minus-sign", lib = "glyphicon")),width = 1)
                    ))
    )
  })
  
  lapply(0:40, function(i) {
    observeEvent(input[[paste0('button',i)]], {
      removeUI(immediate = T,
        selector = paste0("#myrow",i),multiple = FALSE  #"div:has(#myrow)"
      )
    })
  })
  
  ##########################################  
  ###------------- Table Data -----------###  
  ##########################################
  
  expenseData <- reactive({
   df <- data.frame(category='housing',value=input$housing,timeframe=input$housingTimeSelector,button=0)
    for (i in 0:40){
      cur_df <-  data.frame(category=input[[paste0('category_txt',i)]],value=input[[paste0('value_txt',i)]],timeframe=input[[paste0('timeframe_txt',i)]],button=input[[paste0('button',i)]] )
      df <- rbind(df,cur_df)
    }
   dplyr::filter(df,button==0,value>0,nchar(as.character(category))>0) %>% 
     dplyr::mutate(value_annual = ifelse(timeframe=='Annual',value,value*12))
  })
  
  year_data <- reactive({
    transform_data_for_year_plot(expenses = expenseData(),annual_income = incomeYearly(),tax_rate = .3)
  })
  
  output$year_plot <- renderPlotly({
    year_plot_function(year_data())
  })
  
  #output$txt1 <- renderText({ input$category_txt1 }) #[[paste0('category_txt',1)]]
  
}

shinyApp(ui, server)
