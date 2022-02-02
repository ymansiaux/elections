#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @import sf
#' @importFrom shinyYM closeWaiter
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  options(datatable.print.class = TRUE)
  options(bitmapType = "cairo")
  
  observe(closeWaiter(TRUE, 3))
  
  
  ### PARTIE BDXMETROIDENTITY ###
  rv <- reactiveValues()
  rv$theme <- "light"

  output$my_logo <- renderUI({
    if (rv$theme == "light") {
      tags$img(src = "www/datalab-logo-lightmode.png", width = "150px")
    } else if (rv$theme == "dark") {
      tags$img(src = "www/datalab-logo-darkmode.png", width = "150px")
    }
  })
  
}
