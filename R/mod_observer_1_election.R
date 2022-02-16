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
#' @importFrom leaflet leafletOutput createLeafletMap renderLeaflet awesomeIcons leaflet addTiles setView addAwesomeMarkers addPolygons highlightOptions leafletProxy setView

mod_observer_1_election_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    sidebarLayout(
      
      sidebarPanel(
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
        tabsetPanel(
          tabPanel("Plot", 
                   fluidRow(
                     column(width = 2,
                            radioButtons(inputId = ns("numero_scrutin"),
                                         label = "Choisir un scrutin",
                                         choiceNames = "Aucune élection sélectionnée",
                                         choiceValues = ""),
                            
                            selectizeInput(
                              inputId = ns("niveau_geo_restitution"),
                              label = "Niveau de restitution",
                              choices = c("Bureau de vote" = "id_bureau", "Lieu de vote" = "id_lieu"),
                              multiple = FALSE,
                              options = list(deselectBehavior = "top")
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(width = 10,
                            plotOutput(ns("plot1")),
                            plotOutput(ns("plot2"))
                     )
                   )
          ), 
          tabPanel("Map", leafletOutput(ns("myBVmap"))
          ), 
          tabPanel("Plot2", 
                   plotOutput(ns("plot3")),
                   plotOutput(ns("plot4"))
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
        subset(type_election %in% input$type_elections) %>% 
        select(annee_election) %>% 
        unique %>%
        roworder(annee_election) %>% 
        pull()
      
    })
    
    
    commune_elections <- reactive({
      req(data_elections$data)
      req(input$type_elections)
      req(input$annee_elections)
      
      data_elections$data %>%
        subset(type_election %in% input$type_elections & annee_election %in% input$annee_elections) %>% 
        select(code_insee) %>% 
        unique %>%
        roworder(code_insee) %>% 
        pull()
      
    })
    
    
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
    
    observeEvent(list(input$type_elections, input$annee_elections), {
      updateSelectizeInput(session,
                           inputId = "commune_elections",
                           choices = commune_elections(),
                           server = TRUE
      )
    })
    
    election_selectionnee <- reactive({
      
      req(input$annee_elections)
      req(input$type_elections)
      req(input$commune_elections)
      
      data_elections$data %>%
        subset(type_election %in% input$type_elections &
                 annee_election %in% input$annee_elections &
                 code_insee %in% input$commune_elections) %>%
        mutate(numero_tour = ifelse(is.na(numero_tour), 1, numero_tour))
      
    })
    
    election_selectionnee_d <- debounce(election_selectionnee, 500)
  
    observeEvent(election_selectionnee_d(), {
      updateRadioButtons(session,
                         inputId = "numero_scrutin",
                         choiceNames = paste("Tour", sort(unique(election_selectionnee_d()$numero_tour))),
                         choiceValues = sort(unique(election_selectionnee_d()$numero_tour))
      )
    })
   
    election_selectionnee_tour_selectionne <- reactive({
      req(input$annee_elections)
      req(input$type_elections)
      req(input$commune_elections)      
      req(input$numero_scrutin)
      
      election_selectionnee_d() %>%
        subset(numero_tour %in% input$numero_scrutin)
    })
    
    election_selectionnee_tour_selectionne_d <- debounce(election_selectionnee_tour_selectionne, 500)
    if (debug_whereami) whereami::cat_where(where = whereami::whereami())
    observe(print(election_selectionnee_d()))
    observe(print(election_selectionnee_tour_selectionne_d()))
    
    
    
  })
}

## To be copied in the UI
# mod_observer_1_election_ui("observer_1_election_ui_1")

## To be copied in the server
# mod_observer_1_election_server("observer_1_election_ui_1")
