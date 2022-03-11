#' observer_1_election_resultats_carto_candidat_vainqueur UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom glue glue
mod_observer_1_election_resultats_globaux_carto_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(width = 12,
             div(class = "container",
                 style = "display:flex;
        flex-direction : column;
        justify-content: space-evenly",
                 
                 div(class ="title_crazy title_container",
                     div(icon(name="democrat", class = "icon_title")),
                     div(h2("Vainqueur par unité géo.", class = "text-uppercase")),
                     div(icon(name="democrat", class = "icon_title"))
                 ),
                 
                 div(class = "map_container",
                     div(class = "map", id = ns("map"),
                       leafletOutput(ns("carto_resultats"), height = 800)
                     ),
                     div(class = "centered", id = ns("message_absence_donnees_carto"),
                         h1("Les données de localisation des bureaux ne sont pas disponibles pour ce scrutin ou cette commune")
                     )
                     
                 ),
                 
                 div(
                   class = "container",
                   style = "display:flex;
        flex-direction : row;
        justify-content: flex-start",
                   
                   div(
                     radioButtons(inputId = ns("numero_scrutin"),
                                  label = "Choisir un scrutin",
                                  choiceNames = "Aucune élection sélectionnée",
                                  choiceValues = "")
                   ),
                   
                   div(
                     selectizeInput(
                       inputId = ns("niveau_geo_restitution"),
                       label = "Niveau de restitution",
                       choices = c("Bureau de vote" = "id_bureau", "Lieu de vote" = "id_lieu"),
                       multiple = FALSE,
                       options = list(deselectBehavior = "top")
                     )
                   )
                 )
                 
                 
             )
      )
    )
  )
}

#' observer_1_election_resultats_carto_candidat_vainqueur Server Functions
#'
#' @noRd 
mod_observer_1_election_resultats_globaux_carto_server <- function(id, election_selectionnee_d){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$pause, browser())
    
    observe({
      if(!election_selectionnee_d()$annee_election[1] %in% annees_elections_avec_donnees_geo | 
         !election_selectionnee_d()$code_insee[1] %in% communes_elections_avec_donnees_geo) {
        
        runjs(glue('$("#{ns("map")}").addClass("map_with_opacity");'));
        runjs(glue('$("#{ns("message_absence_donnees_carto")}").show();'));
    
      } else {
        
        runjs(glue('$("#{ns("map")}").removeClass("map_with_opacity");'));
        runjs(glue('$("#{ns("message_absence_donnees_carto")}").hide();'));
        
        }
    })
    
    observeEvent(election_selectionnee_d(), {
      updateRadioButtons(session,
                         inputId = "numero_scrutin",
                         choiceNames = paste("Tour", sort(unique(election_selectionnee_d()$numero_tour))),
                         choiceValues = sort(unique(election_selectionnee_d()$numero_tour))
      )
    })
    
    election_selectionnee_tour_selectionne <- reactive({
      
      req(input$numero_scrutin)
      
      election_selectionnee_d() %>%
        filter(numero_tour %in% input$numero_scrutin)
    })
    
    election_selectionnee_tour_selectionne_d <- debounce(election_selectionnee_tour_selectionne, 500)
    
    
    
    #### CARTO RESULTATS DETAILLES
    # 
    # ,
    # input$niveau_geo_restitution
    # 
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
    
    
    ## CARTO OK BV
    donnees_carto_vainqueur_by_unite_geo <- reactive({ 
      ## FONCTIONNE TB AVEC LES MUNICIPALES DE 2020
      ## VOIR UNE FOIS QUE L'HISTO SERA DISPO
      data <-  compute_resultats_elections(data = election_selectionnee_tour_selectionne_d(),
                                           type = "participation",
                                           grouping_vars = c(
                                             "nom_election", "type_election", "annee_election",
                                             "nom_candidat", "nom", "nom_candidat_short",
                                             "numero_tour", 
                                             input$niveau_geo_restitution)) %>% 
        mutate_at(vars(input$niveau_geo_restitution),as.character)
      
      winner <- data %>% 
        group_by(!!!syms(c("numero_tour", input$niveau_geo_restitution))) %>% 
        mutate(pctmax = max(pct)) %>%
        filter(pctmax == pct) %>% 
        ungroup()
      
      
      if(input$niveau_geo_restitution == "id_bureau") {
        donnees_geo_winner <- merge(donnees_geo_selectionnees(),
                                    winner,
                                    by.x = "code", by.y = input$niveau_geo_restitution)
        
      } else {
        donnees_geo_winner <- merge(donnees_geo_selectionnees(),
                                    winner,
                                    by.x = "rs_el_lieuvote_p", by.y = input$niveau_geo_restitution)
        
      }
      
      donnees_geo_winner
      
    })
    
    output$carto_resultats <- renderLeaflet({
      #####################################
      # création de la palette de couleur #
      ####################################
      
      # quels sont les candidats vainqueurs ?
      candidats_vainqueurs_couleurs <- data.frame(
        "nom_candidat" = sort(unique(donnees_carto_vainqueur_by_unite_geo()$nom_candidat)),
        "couleur" = viridis::viridis_pal()(length(unique(donnees_carto_vainqueur_by_unite_geo()$nom_candidat))))
      
      donnees_geo_winner <- merge(donnees_carto_vainqueur_by_unite_geo(),
                                  candidats_vainqueurs_couleurs,
                                  by = "nom_candidat") %>% 
        mutate(pct = pct * 100)
      
      
      # donnees_geo_selectionnees()[!donnees_geo_selectionnees()$code%in% data$id_bureau,]
      
      popup <-  paste0("<strong>Zone: </strong>",
                       donnees_geo_winner$libelle,
                       "<br><strong>Candidat: </strong>",
                       donnees_geo_winner$nom_candidat,
                       "<br><strong>% recueillis: </strong>",
                       sprintf("%.2f",donnees_geo_winner$pct))
      
      leaflet(donnees_geo_winner) %>% 
        addTiles() %>% 
        setView(zoom = 11.5, lat = 44.859684, lng = -0.568365) %>%
        addPolygons(fillColor = ~couleur, color = "grey",
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = .8,
                    popup = popup,
                    highlightOptions = leaflet::highlightOptions(color = "black", weight = 2,
                                                                 bringToFront = TRUE))
    })
    
  })
}

## To be copied in the UI
# mod_observer_1_election_resultats_carto_candidat_vainqueur_ui("observer_1_election_resultats_carto_candidat_vainqueur_ui_1")

## To be copied in the server
# mod_observer_1_election_resultats_carto_candidat_vainqueur_server("observer_1_election_resultats_carto_candidat_vainqueur_ui_1")
