#' observer_1_election UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_observer_1_election_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    selectizeInput(
      inputId = ns("type_elections"),
      label = "Type d'election",
      choices = NULL,
      multiple = FALSE,
      options = list(deselectBehavior = "top")
    ),
    
    selectizeInput(
      inputId = ns("annee_elections"),
      label = "Annee de l'election",
      choices = NULL,
      multiple = FALSE,
      options = list(deselectBehavior = "top")
    )
    
  )
}

#' observer_1_election Server Functions
#'
#' @noRd 
mod_observer_1_election_server <- function(id, data_elections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    ######
    type_elections <- reactive({
      req(data_elections$data)
      sort(
        unique(
          data_elections$data$type_election
        )
      )
    }) #%>% debounce(100)
    
    annee_elections <- reactive({
      req(data_elections$data)
      req(input$type_elections)
      
      sort(
        unique(
          unlist(
            data_elections$data[type_election %in% input$type_elections, "annee_election"])
        )
      )
    })# %>% debounce(1000)
    
    
    observe({
      updateSelectizeInput(session,
                           inputId = "type_elections",
                           choices = type_elections(),
                           server = TRUE
      )
    })
    
    observeEvent(input$type_elections, {
      updateSelectizeInput(session,
                           inputId = "annee_elections",
                           choices = annee_elections(),
                           server = TRUE
      )
    })
    
    
    
    
    
  })
}

## To be copied in the UI
# mod_observer_1_election_ui("observer_1_election_ui_1")

## To be copied in the server
# mod_observer_1_election_server("observer_1_election_ui_1")
