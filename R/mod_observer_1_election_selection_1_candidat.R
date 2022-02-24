#' observer_1_election_selection_1_candidat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_observer_1_election_selection_1_candidat_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("pause"), "pause"),
    
    fluidRow(
      column(width = 2,
             
             selectizeInput(
               inputId = ns("candidat"),
               label = "Sélectionner un candidat",
               choices = NULL,
               multiple = FALSE,
               options = list(deselectBehavior = "top")
             ),
             
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
      column(width = 12,
             leafletOutput(ns("carto_resultats")),
             plotOutput(ns("graphique_resultats"))
      )
    )
  )
}

#' observer_1_election_selection_1_candidat Server Functions
#'
#' @noRd 
mod_observer_1_election_selection_1_candidat_server <- function(id, election_selectionnee_d){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$pause, browser())
    
    
    observeEvent(election_selectionnee_d(), {
      
      updateSelectizeInput(session,
                           inputId = "candidat",
                           choices = sort(unique(election_selectionnee_d()$nom_candidat)),
                           selected = sort(unique(election_selectionnee_d()$nom_candidat))[1],
                           server = TRUE
      )
      
      
      
      
    })
    
    observeEvent(input$candidat, {
      
      tours_dispos <- election_selectionnee_d() %>% 
        filter(nom_candidat %in% input$candidat) %>% 
        select(numero_tour) %>% 
        distinct() %>% 
        pull()
      
      
      updateRadioButtons(session,
                         inputId = "numero_scrutin",
                         choiceNames = paste("Tour", sort(tours_dispos)),
                         choiceValues = sort(tours_dispos)
      )
      
    })
    
    
    donnees_elections_candidat <- reactive({
      
      req(input$candidat)
      req(input$numero_scrutin)
      
      election_selectionnee_d() %>% 
        filter(nom_candidat %in% input$candidat & numero_tour %in% input$numero_scrutin)
      
    })
    
    donnees_elections_candidat_d <- debounce(donnees_elections_candidat, 500)
    
    
    resultats_elections_candidat <- reactive({
      compute_resultats_elections(data = donnees_elections_candidat_d(),
                                  type = "participation",
                                  grouping_vars = c(
                                    "nom_election", "type_election", "annee_election",
                                    "nom_candidat", "nom", "nom_candidat_short",
                                    "numero_tour", 
                                    input$niveau_geo_restitution)) %>% 
        mutate_at(vars(input$niveau_geo_restitution),as.character)
      
    })
    
    
    donnees_geo_selectionnees <- reactive({
      if(input$niveau_geo_restitution == "id_bureau") {
        elections::bureaux_votes_bdx
      } else {
        elections::bureaux_votes_bdx %>% 
          group_by(libelle, rs_el_lieuvote_p) %>% 
          summarise(geometry = st_union(geometry)) %>% 
          ungroup()
      }
      
    })
    
    donnees_cartos <- reactive({ 
      
      if(input$niveau_geo_restitution == "id_bureau") {
        merge(donnees_geo_selectionnees(),
              resultats_elections_candidat(),
              by.x = "code", by.y = input$niveau_geo_restitution)
        
      } else {
        merge(donnees_geo_selectionnees(),
              resultats_elections_candidat(),
              by.x = "rs_el_lieuvote_p", by.y = input$niveau_geo_restitution)
        
      }
      
    })
    
    output$carto_resultats <- renderLeaflet({
      #####################################
      # création de la palette de couleur #
      ####################################
      
      # quels sont les candidats vainqueurs ?
      donnees_cartos <- donnees_cartos() %>% 
        mutate(couleur = sample(viridis::viridis_pal()(10), 1)) %>% 
        mutate(pct = pct * 100)
      
      
      # donnees_geo_selectionnees()[!donnees_geo_selectionnees()$code%in% data$id_bureau,]
      
      popup <-  paste0("<strong>Zone: </strong>",
                       donnees_cartos$libelle,
                       "<br><strong>Candidat: </strong>",
                       donnees_cartos$nom_candidat,
                       "<br><strong>% recueillis: </strong>",
                       sprintf("%.2f",donnees_cartos$pct))
      
      pal <- colorNumeric(palette = "YlOrRd", domain = donnees_cartos$pct)
      
      leaflet(donnees_cartos) %>% 
        addTiles() %>% 
        setView(zoom = 11.5, lat = 44.859684, lng = -0.568365) %>%
        # addPolygons(fillColor = ~couleur, color = "grey",
        addPolygons(fillColor = ~pal(pct), color = "grey",
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = .8,
                    popup = popup,
                    highlightOptions = leaflet::highlightOptions(color = "black", weight = 2,
                                                                 bringToFront = TRUE)) %>% 
        addLegend(pal = pal, values = ~pct, group = "circles", position = "topright") 
    })
    
    output$carto_resultats <- renderLeaflet({
      #####################################
      # création de la palette de couleur #
      ####################################
      
      # quels sont les candidats vainqueurs ?
      donnees_cartos <- donnees_cartos() %>% 
        mutate(couleur = sample(viridis::viridis_pal()(10), 1)) %>% 
        mutate(pct = pct * 100)
      
      
      # donnees_geo_selectionnees()[!donnees_geo_selectionnees()$code%in% data$id_bureau,]
      
      popup <-  paste0("<strong>Zone: </strong>",
                       donnees_cartos$libelle,
                       "<br><strong>Candidat: </strong>",
                       donnees_cartos$nom_candidat,
                       "<br><strong>% recueillis: </strong>",
                       sprintf("%.2f",donnees_cartos$pct))
      
      pal <- colorNumeric(palette = "YlOrRd", domain = donnees_cartos$pct)
      
      leaflet(donnees_cartos) %>% 
        addTiles() %>% 
        setView(zoom = 11.5, lat = 44.859684, lng = -0.568365) %>%
        # addPolygons(fillColor = ~couleur, color = "grey",
        addPolygons(fillColor = ~pal(pct), color = "grey",
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = .8,
                    popup = popup,
                    highlightOptions = leaflet::highlightOptions(color = "black", weight = 2,
                                                                 bringToFront = TRUE)) %>% 
        addLegend(pal = pal, values = ~pct, group = "circles", position = "topright") 
    })
    
    output$graphique_resultats <- renderPlot({
      req(resultats_elections_candidat())
      
      graphique_resultats_election(data = resultats_elections_candidat(), x = !!rlang::sym(input$niveau_geo_restitution), 
                                   y = pct, fill = nom_candidat, facet = FALSE, theme_fun = theme_elections())
      
    })
    
  })
}

## To be copied in the UI
# mod_observer_1_election_selection_1_candidat_ui("observer_1_election_selection_1_candidat_ui_1")

## To be copied in the server
# mod_observer_1_election_selection_1_candidat_server("observer_1_election_selection_1_candidat_ui_1")
