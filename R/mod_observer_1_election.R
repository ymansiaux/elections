#' observer_1_election UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2
#' @importFrom scales percent
#' @importFrom dplyr pull
#' @importFrom bdxmetroidentity scale_fill_bdxmetro_discrete
#' @importFrom ggtext element_markdown
#' @importFrom viridis scale_fill_viridis
#' @importFrom stringr str_trim str_replace
#' @importFrom leaflet leafletOutput createLeafletMap renderLeaflet awesomeIcons leaflet addTiles setView addAwesomeMarkers addPolygons highlightOptions leafletProxy setView colorNumeric addLegend

mod_observer_1_election_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    sidebarLayout(
      
      sidebarPanel(
        width = 2,
        
        fluidRow(
          
          column(width = 12,
                 
                 actionButton(ns("pause"), "pause"),
                 
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
                 ),
                 
                 selectizeInput(
                   inputId = ns("commune_elections"),
                   label = "Commune",
                   choices = NULL,
                   multiple = FALSE,
                   options = list(deselectBehavior = "top")
                 )
          )
        )
        
      ),
      
      mainPanel(
        width = 10,
        
        tabsetPanel( type = "pills",
                     
          tabPanel("Plot",
                   mod_observer_1_election_resultats_globaux_ui(ns("observer_1_election_resultats_globaux_ui_1")),
                   mod_observer_1_election_resultats_carto_candidat_vainqueur_ui(ns("observer_1_election_resultats_carto_candidat_vainqueur_ui_1"))
          ),
          tabPanel("Map", 
                   mod_observer_1_election_selection_LV_sur_carte_ui(ns("observer_1_election_selection_LV_sur_carte_ui_1"))
          ), 
          tabPanel("Plot2", 
                   mod_observer_1_election_selection_1_candidat_ui(ns("observer_1_election_selection_1_candidat_ui_1"))
          )
        )
        
      )
    )
  )
}

#' observer_1_election Server Functions
#'
#' @noRd 
mod_observer_1_election_server <- function(id, data_elections, debug_whereami){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$pause, browser())
    
    ######
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
    
    election_selectionnee_d <- debounce(election_selectionnee, 500)
    
    ## SOUS MODULES
    mod_observer_1_election_resultats_globaux_server("observer_1_election_resultats_globaux_ui_1", election_selectionnee_d)
    mod_observer_1_election_resultats_carto_candidat_vainqueur_server("observer_1_election_resultats_carto_candidat_vainqueur_ui_1", election_selectionnee_d)
    
    mod_observer_1_election_selection_LV_sur_carte_server("observer_1_election_selection_LV_sur_carte_ui_1", election_selectionnee_d)
    
    mod_observer_1_election_selection_1_candidat_server("observer_1_election_selection_1_candidat_ui_1", election_selectionnee_d)
  })
}

## To be copied in the UI
# mod_observer_1_election_ui("observer_1_election_ui_1")

## To be copied in the server
# mod_observer_1_election_server("observer_1_election_ui_1")
