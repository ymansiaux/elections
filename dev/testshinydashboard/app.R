#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
body <- dashboardBody(
  fluidRow(
    box(title = "Box title", "Box content"),
    box(status = "warning", "Box content")
  ),
  
  fluidRow(
    box(
      title = "Title 1", width = 4, solidHeader = TRUE, status = "primary",
      "Box content"
    ),
    box(
      title = "Title 2", width = 4, solidHeader = TRUE,
      "Box content"
    ),
    box(
      title = "Title 1", width = 4, solidHeader = TRUE, status = "warning",
      "Box content"
    )
  ),
  
  fluidRow(
    box(
      width = 4, background = "black",
      "A box with a solid black background"
    ),
    box(
      title = "Title 5", width = 4, background = "light-blue",
      "A box with a solid light-blue background"
    ),
    box(
      title = "Title 6",width = 4, background = "maroon",
      "A box with a solid maroon background"
    )
  )
)

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "Row layout"),
  dashboardSidebar(),
  body
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      

        # draw the histogram with the specified number of bins
        hist(1:1000, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
