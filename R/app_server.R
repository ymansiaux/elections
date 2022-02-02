#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @import sf
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  options(datatable.print.class = TRUE)
  options(bitmapType = "cairo")
}
