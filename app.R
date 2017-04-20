
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Dynamically insert and remove Shiny UI objects"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      )
      ,
      
      # Show a plot of the generated distribution
      mainPanel(
        tags$div(id = 'placeholder') 
        ,actionButton("add", "Add Line",icon = icon("glyphicon glyphicon-plus-sign", lib = "glyphicon"))
      )
   )
)

server <- function(input, output) {
   
  #dynamically add row of inputs
  observeEvent(input$add, {
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(id=paste0('myrow',input$add),fluidRow(column(textInput(paste0("category_txt", input$add),label = NULL),width = 3)
                                                                               ,column(numericInput(paste0("value_txt", input$add),label = NULL,value = 0),width = 3)
                                                                               ,column(actionButton(paste0("button", input$add),label = NULL,icon = icon("glyphicon glyphicon-minus-sign", lib = "glyphicon")),width = 1)
      ))
    )
  })
  
  #dynamically remove row of inputs
  lapply(0:50, function(i) {            #max of 50 buttons added and deleted (the value 50 was chosen arbitrarily and can be changed)
    observeEvent(input[[paste0('button',i)]], {
      removeUI(immediate = T,
               selector = paste0("#myrow",i),multiple = FALSE
      )
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

