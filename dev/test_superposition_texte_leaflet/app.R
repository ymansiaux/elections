#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("css.css"),
  
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      div(class = "container",
          leafletOutput("distPlot"),
          div(class = "centered",
              h1("titi")
          )
          
      ),
      
      tagList(
        tags$div(
          class = "container",
          tags$img(
            src = "https://www.w3schools.com/howto/img_snow_wide.jpg",
            alt = "Snow",
            style = "width:100%;"
          ),
          tags$div(
            class = "bottom-left",
            "Bottom Left"
          ),
          tags$div(
            class = "top-left",
            "Top Left"
          ),
          tags$div(
            class = "top-right",
            "Top Right"
          ),
          tags$div(
            class = "bottom-right",
            "Bottom Right"
          ),
          tags$div(
            class = "centered",
            "Centered"
          )
        )
      )
      # https://www.w3schools.com/howto/howto_css_image_text.asp
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderLeaflet({
    # generate bins based on input$bins from ui.R
    leaflet() %>% addTiles() %>%         setView(zoom = 11.5, lat = 44.859684, lng = -0.568365) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
