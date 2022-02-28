#' accueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_accueil_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' accueil Server Functions
#'
#' @noRd 
mod_accueil_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_accueil_ui("accueil_ui_1")
    
## To be copied in the server
# mod_accueil_server("accueil_ui_1")
