library(shiny)

# setup ui object
ui <- 
  
  pageWithSidebar(
    headerPanel('Test IWIMS data analysis'),
    sidebarPanel(
      selectInput(inputId = 'shop', label = 'Shop:', choices = 'CA')
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )

# setup server object
server <- function(input, output) {
  
  
 
}




shinyApp(ui = ui, server = server)