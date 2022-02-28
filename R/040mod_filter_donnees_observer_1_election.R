#' filter_donnees_observer_1_election UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filter_donnees_observer_1_election_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             div(class = "container",
                 style = "display:flex;
        flex-direction : row;
        justify-content: space-evenly",
                 
                  div(
                    actionButton(ns("pause"), "pause")
                  ),
                 div(
                   selectizeInput(
                     inputId = ns("type_elections"),
                     label = "Type d'election",
                     choices = NULL,
                     multiple = FALSE,
                     options = list(deselectBehavior = "top")
                   )
                 ),
                 div(
                   selectizeInput(
                     inputId = ns("annee_elections"),
                     label = "Annee de l'election",
                     choices = NULL,
                     multiple = FALSE,
                     options = list(deselectBehavior = "top")
                   )
                 ),
                 div(
                   selectizeInput(
                     inputId = ns("commune_elections"),
                     label = "Commune",
                     choices = NULL,
                     multiple = FALSE,
                     options = list(deselectBehavior = "top")
                   ),
                   
                 )
                 
             )
      )
    )
    
   
  )
}
    
#' filter_donnees_observer_1_election Server Functions
#'
#' @noRd 
mod_filter_donnees_observer_1_election_server <- function(id, data_elections){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
 
    
    type_elections <- reactive({
      req(data_elections$data)
      sort(
        unique(
          data_elections$data$type_election
        )
      )
    })
    
    annee_elections <- reactive({
      req(data_elections$data)
      req(input$type_elections)
      
      data_elections$data %>%
        filter(type_election %in% input$type_elections) %>% 
        select(annee_election) %>% 
        distinct %>%
        arrange(annee_election) %>% 
        pull()
      
    })
    
    
    commune_elections <- reactive({
      req(data_elections$data)
      req(input$type_elections)
      req(input$annee_elections)
      
      data_elections$data %>%
        filter(type_election %in% input$type_elections & annee_election %in% input$annee_elections) %>% 
        select(code_insee) %>% 
        distinct %>%
        arrange(code_insee) %>% 
        pull()
      
    })
    
    
    observe({
      updateSelectizeInput(session,
                           inputId = "type_elections",
                           choices = type_elections(),
                           selected = "Municipale",
                           server = TRUE
      )
    })
    
    observeEvent(input$type_elections, {
      updateSelectizeInput(session,
                           inputId = "annee_elections",
                           choices = annee_elections(),
                           selected = 2020,
                           server = TRUE
      )
      
    })
    
    observeEvent(list(input$type_elections, input$annee_elections), {
      updateSelectizeInput(session,
                           inputId = "commune_elections",
                           choices = commune_elections(),
                           selected = 33063,
                           server = TRUE
      )
    })
    
    election_selectionnee <- reactive({
      
      req(input$annee_elections)
      req(input$type_elections)
      req(input$commune_elections)
      
      data_elections$data %>%
        filter(type_election %in% input$type_elections &
                 annee_election %in% input$annee_elections &
                 code_insee %in% input$commune_elections) %>%
        mutate(numero_tour = ifelse(is.na(numero_tour), 1, numero_tour))
      
    })
    
    return(election_selectionnee)
  })
}
    
## To be copied in the UI
# mod_filter_donnees_observer_1_election_ui("filter_donnees_observer_1_election_ui_1")
    
## To be copied in the server
# mod_filter_donnees_observer_1_election_server("filter_donnees_observer_1_election_ui_1")
