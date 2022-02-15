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
             plotOutput(ns("plot1")),
             plotOutput(ns("plot2")),
             leafletOutput(ns("myBVmap")),
             plotOutput(ns("plot3")),
             plotOutput(ns("plot4"))
             
      )
      # radioButtons(inputId = ns("tour_election
    )
  )
}

#' observer_1_election Server Functions
#'
#' @noRd 
mod_observer_1_election_server <- function(id, data_elections){
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
    }) #%>% debounce(100)
    
    annee_elections <- reactive({
      req(data_elections$data)
      req(input$type_elections)
      
      data_elections$data %>%
        fsubset(type_election %in% input$type_elections) %>% 
        fselect(annee_election) %>% 
        funique %>%
        roworder(annee_election) %>% 
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
    
    election_selectionnee <- reactive({
      
      req(input$annee_elections)
      req(input$type_elections)
      
      data_elections$data %>%
        fsubset(type_election %in% input$type_elections & annee_election %in% input$annee_elections) %>% 
        mutate(numero_tour = ifelse(is.na(numero_tour), 1, numero_tour))
      
      
    }) 
    
    results_by_tour_by_candidate <- reactive({
      
      req(isTruthy(election_selectionnee()))
      
      compute_resultats_elections(data = election_selectionnee(), 
                                  type = "participation", 
                                  nom_election, type_election, annee_election, 
                                  numero_tour, nom_candidat, nom, nom_candidat_short
      )
      
      
    }) %>% debounce(500)
    
    
    output$plot1 <- renderPlot({
      # pivoter les axes des x
      # faire un joli theme
      # changer les couleurs
      # interactif
      
      graphique_resultats_election(data = results_by_tour_by_candidate(), x = nom_candidat_short, y = pct, fill = nom_candidat)
      
    })
    
    abstention <- reactive({
      
      req(isTruthy(election_selectionnee()))
      compute_resultats_elections(data = election_selectionnee(),
                                  type = "abstention",
                                  nom_election, type_election, annee_election, numero_tour
      ) 
      
    })
    
    
    output$plot2 <- renderPlot({
      graphique_resultats_election(data = abstention(), x = numero_tour, y = pct, fill = numero_tour)
      
      
    })
    
    # Partie carto
    #### Carte initiale
    myBVmap <- createLeafletMap(session, 'myBVmap')
    
    session$onFlushed(once = T, function() {
      
      output$myBVmap <- renderLeaflet({
        
        popup_markers <- paste("<b>Lieu de vote</b>", elections::lieux_votes_bdx$libelle)
        
        icons <- awesomeIcons(
          icon = 'ios-close',
          iconColor = color_vector(),
          library = 'ion',
          markerColor = color_vector()
        )
        
        leaflet(elections::lieux_votes_bdx) %>% 
          addTiles() %>% 
          setView(zoom = 11.5, lat =44.859684, lng=-0.568365) %>% 
          addAwesomeMarkers(popup = popup_markers, layerId = elections::lieux_votes_bdx$gid, icon = icons) %>% 
          addPolygons( data = elections::secteurs_votes_bdx,
                       weight = 1, smoothFactor = 0.5,
                       opacity = .75, fillOpacity = .2,
                       highlightOptions = highlightOptions(color = "black", weight = 2,
                                                           bringToFront = TRUE))
        
        
      })
    })
    
    observeEvent(input$myBVmap_marker_click, { 
      p <- input$myBVmap_marker_click  # typo was on this line
      print(p)
      
      leafletProxy("myBVmap") %>% 
        setView(zoom = input$myBVmap_zoom, lng = input$myBVmap$lng, lat = input$myBVmap$lat)
      
    })
    
    
    clickedIds <- reactiveValues(ids = vector())
    
    color_vector <- reactive({
      
      if(is.null(input$myBVmap_marker_click)) rep("blue", length(elections::lieux_votes_bdx$gid))
      else {
        clicked_marker <- input$myBVmap_marker_click$id 
        
        ifelse(elections::lieux_votes_bdx$gid == clicked_marker, "red", "blue")
      }
      
    })
    
    filtered_data_by_BV <- reactive({
      ## LIMITER CETTE ANALYSE AUX PRESIDENTIELLES A PARTIR DE 2017
      req(input$myBVmap_marker_click)
      
      # on récupère les bv associés au lv sélectionné
      bv_selectionnes <- elections::bureaux_votes_bdx[elections::bureaux_votes_bdx$rs_el_lieuvote_p %in% input$myBVmap_marker_click$id, ]
      
      election_selectionnee() %>% 
        fsubset(id_bureau %in% bv_selectionnes$code)
    })
    
    resultats_by_BV <- reactive({
      
      req(isTruthy(filtered_data_by_BV()))
      
      compute_resultats_elections(data = filtered_data_by_BV(), 
                                  type = "participation", 
                                  nom_election, type_election, annee_election, 
                                  numero_tour, nom_candidat, nom, nom_candidat_short,
                                  id_bureau    
      )
      
    }) %>% debounce(500)
    
    
    resultats_by_LV <- reactive({
      
      req(isTruthy(filtered_data_by_BV()))
      
      compute_resultats_elections(data = filtered_data_by_BV(), 
                                  type = "participation", 
                                  nom_election, type_election, annee_election, 
                                  numero_tour, nom_candidat, nom, nom_candidat_short,
                                  nom_lieu
    )
    
    })
  
  output$plot3 <- renderPlot({
    graphique_resultats_election(data = resultats_by_BV(), x = nom_candidat_short, y = pct, fill = nom_candidat)
    
    
  })
  
  
  output$plot4 <- renderPlot({
    graphique_resultats_election(data = resultats_by_LV(), x = nom_candidat_short, y = pct, fill = nom_candidat)
    
    
  })
  
  
  })
}

## To be copied in the UI
# mod_observer_1_election_ui("observer_1_election_ui_1")

## To be copied in the server
# mod_observer_1_election_server("observer_1_election_ui_1")
