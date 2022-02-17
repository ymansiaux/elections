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
                            plotOutput(ns("graphique_resultats")),
                            plotOutput(ns("graphique_abstention"))
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
        fsubset(type_election %in% input$type_elections) %>% 
        fselect(annee_election) %>% 
        funique %>%
        roworder(annee_election) %>% 
        pull()
      
    })
    
    
    commune_elections <- reactive({
      req(data_elections$data)
      req(input$type_elections)
      req(input$annee_elections)
      
      data_elections$data %>%
        subset(type_election %in% input$type_elections & annee_election %in% input$annee_elections) %>% 
        fselect(code_insee) %>% 
        funique %>%
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
        fsubset(type_election %in% input$type_elections &
                  annee_election %in% input$annee_elections &
                  code_insee %in% input$commune_elections) %>%
        fmutate(numero_tour = ifelse(is.na(numero_tour), 1, numero_tour))
      
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
    
    ### GRAPHIQUE RESULTATS AGREGES 
    output$graphique_resultats <- renderPlot({
      # pivoter les axes des x
      # faire un joli theme
      # changer les couleurs
      # interactif
      
      
      compute_resultats_elections(data = election_selectionnee_d(), 
                                  type = "participation", 
                                  nom_election, type_election, annee_election, 
                                  numero_tour, nom_candidat, nom, nom_candidat_short) %>% 
        
        graphique_resultats_election(data = ., x = nom_candidat_short, y = pct, fill = nom_candidat)
      
    })
    
    
    output$graphique_abstention <- renderPlot({
      # pivoter les axes des x
      # faire un joli theme
      # changer les couleurs
      # interactif
      
      compute_resultats_elections(data = election_selectionnee_d(),
                                  type = "abstention",
                                  nom_election, type_election, annee_election, numero_tour)  %>% 
        
        graphique_resultats_election(data = ., x = numero_tour, y = pct, fill = numero_tour)
      
    })
    
    #### CARTO RESULTATS DETAILLES
    # 
    # ,
    # input$niveau_geo_restitution
    # 
    donnees_geo_selectionnees <- reactive({
      
      # if(input$niveau_geo_restitution == "id_bureau") {
        elections::bureaux_votes_bdx
      # } else {
      #   elections::lieux_votes_bdx
      # }
      
    })
    
    
    ## CARTO OK BV
    
    donnees_carto_resultats <- reactive({
      
      ## FONCTIONNE TB AVEC LES MUNICIPALES DE 2020
      ## VOIR UNE FOIS QUE L'HISTO SERA DISPO
      data <-  compute_resultats_elections(data = election_selectionnee_tour_selectionne_d(),
                                           type = "participation",
                                           nom_election, type_election, annee_election,
                                           nom_candidat, nom, nom_candidat_short,
                                           numero_tour, 
                                           id_bureau) %>% 
        fmutate(id_bureau = as.character(id_bureau))
      
      winner <- data %>% 
        fgroup_by(numero_tour, id_bureau) %>% 
        fmutate(pctmax = max(pct)) %>%
        fsubset(pctmax == pct)
      
      donnees_geo_winner <- merge(donnees_geo_selectionnees(),
                                  winner,
                                  by.x = "code",
                                  by.y = "id_bureau")
      
      #####################################
      # création de la palette de couleur #
      ####################################
      
      # quels sont les candidats vainqueurs ?
      candidats_vainqueurs_couleurs <- data.frame(
        "nom_candidat" = sort(unique(donnees_geo_winner$nom_candidat)),
        "couleur" = viridis::viridis_pal()(length(unique(donnees_geo_winner$nom_candidat)))
      )
      
      donnees_geo_winner <- merge(donnees_geo_winner,
                                  candidats_vainqueurs_couleurs,
                                  by = "nom_candidat")
      
      
      # donnees_geo_selectionnees()[!donnees_geo_selectionnees()$code%in% data$id_bureau,]
      
      popup <-  paste0("<strong>Zone: </strong>",
                       donnees_geo_winner$libelle,
                       "<br><strong>Candidat: </strong>",
                       donnees_geo_winner$nom_candidat,
                       "<br><strong>% recueillis: </strong>",
                       sprintf("%.2f",donnees_geo_winner$pct * 100))
      
      
      leaflet(donnees_geo_winner) %>% 
        addTiles() %>% 
        setView(zoom = 11.5, lat = 44.859684, lng = -0.568365) %>%
        addPolygons(fillColor = ~couleur, color = "grey",
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = .8,
                    popup = popup,
                    highlightOptions = leaflet::highlightOptions(color = "black", weight = 2,
                                                                 bringToFront = TRUE))
      
      
      ## CARTO OK LV
      
      
      # 
      # 
      # roworder(data, id_bureau) %>% fgroup_by(id_bureau) %>% fmutate(P = fmax(pct)) %>% fsubset(P == pct)
      
      # il nous faut le vainqueur par unité géo
      # 
      # 
      # colnames(data)[colnames(data) == "get(input$niveau_geo_restitution)"] <- input$niveau_geo_restitution
      # 
      # if(input$niveau_geo_restitution == "id_bureau") {
      #   data[, "id_bureau"] <- as.character(data[, "id_bureau"])
      #   inner_join(donnees_geo_selectionnees(), data, by = c("code" = "id_bureau"))
      #   
      # }
      
      
      
    })
    
    
  })
}

## To be copied in the UI
# mod_observer_1_election_ui("observer_1_election_ui_1")

## To be copied in the server
# mod_observer_1_election_server("observer_1_election_ui_1")
