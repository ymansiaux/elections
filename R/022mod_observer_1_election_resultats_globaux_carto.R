#' observer_1_election_resultats_carto_candidat_vainqueur UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_observer_1_election_resultats_globaux_carto_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      # actionButton(ns("pause"), "Poz"),
      column(width = 12,
             div(class = "container",
                 style = "display:flex;
                          flex-direction : column;
                          justify-content: space-evenly",
                 
                 div(class ="title_section title_container",
                     div(icon(name="democrat", class = "icon_title")),
                     div(h2("Vainqueur par unit\u00e9 g\u00e9o.", class = "text-uppercase")),
                     div(icon(name="democrat", class = "icon_title"))
                 ),
                 
                 div(
                   class = "container",
                   style = "display:flex;
                            flex-direction : row;
                            justify-content: space-evenly",
                   
                   div(
                     radioButtons(inputId = ns("numero_scrutin"),
                                  label = "Choisir un scrutin",
                                  inline = TRUE,
                                  choiceNames = "Aucune \u00e9lection s\u00e9lectionn\u00e9e",
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
                   ),
                   
                   div(
                     selectizeInput(
                       inputId = ns("type_resultats"),
                       label = "R\u00e9sultats \u00e0 afficher",
                       choices = c("R\u00e9sultats" = "resultats", "Abstention" = "abstention"),
                       multiple = FALSE,
                       options = list(deselectBehavior = "top")
                     )
                   )
                 ),
                 
                 div(class = "map_container",
                     div(class = "map", id = ns("map"),
                         leafletOutput(ns("carto_resultats"), height = 800)
                     ),
                     div(class = "centered", id = ns("message_absence_donnees_carto"),
                         h1("Les donn\u00e9es de localisation des bureaux ne sont pas disponibles pour ce scrutin ou cette commune")
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
mod_observer_1_election_resultats_globaux_carto_server <- function(id, data_elections, election_selectionnee){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$pause, browser())
    
    observe({
      
      if(is.null(election_selectionnee()))  {
        
        runjs(glue('$(\"#{ns(\"message_absence_donnees_carto\")}\").hide();'));
        
      } else {        
        updateRadioButtons(session,
                           inputId = "numero_scrutin",
                           choiceNames = paste("Tour", sort(unique(data_elections$data[[election_selectionnee()]]$donneesElection$numero_tour))),
                           choiceValues = sort(unique(data_elections$data[[election_selectionnee()]]$donneesElection$numero_tour))
        )
        
        if(is.null(data_elections$data[[election_selectionnee()]]$cartoBV) | 
           is.null(data_elections$data[[election_selectionnee()]]$cartoLV)) {
          
          runjs(glue('$(\"#{ns(\"map\")}\").addClass(\"map_with_opacity\");'));
          runjs(glue('$(\"#{ns(\"message_absence_donnees_carto\")}\").show();'));
          
        } else {
          
          runjs(glue('$(\"#{ns(\"map\")}\").removeClass(\"map_with_opacity\");'));
          runjs(glue('$(\"#{ns(\"message_absence_donnees_carto\")}\").hide();'));
          
        }
      }
    })
    
    
    donnees_geo_selectionnees <- reactive({
      if(input$niveau_geo_restitution == "id_bureau") {
        
        data_elections$data[[election_selectionnee()]]$cartoBV
        
      } else {
        
        if(!is.null(data_elections$data[[election_selectionnee()]]$cartoBV)) {
          
          data_elections$data[[election_selectionnee()]]$cartoBV %>%
            group_by(libelle, rs_el_lieuvote_p) %>%
            summarise(geometry = st_union(geometry)) %>%
            ungroup()
          
        } else {
          NULL
        }
      }
      
    })
    
    
    donnees_carto_vainqueur_by_unite_geo <- reactive({
      
      req(election_selectionnee())
      req(donnees_geo_selectionnees())
      req(input$numero_scrutin)
      req(input$niveau_geo_restitution)
      req(input$type_resultats)
      
      source_resultats <- ifelse(
        input$niveau_geo_restitution == "id_bureau" & input$type_resultats == "resultats",
        "resultatsBV",
        ifelse(input$niveau_geo_restitution == "id_bureau" & input$type_resultats == "abstention",
               "resultatsAbstentionBV",
               ifelse(input$niveau_geo_restitution != "id_bureau" & input$type_resultats == "resultats",
                      "resultatsLV", 
                      "resultatsAbstentionLV"
               )
        )
      )
      
      resultats_elections <- data_elections$data[[election_selectionnee()]][[source_resultats]]
      
      winner <- resultats_elections %>%
        filter(numero_tour %in% input$numero_scrutin) %>% 
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
      
      if(input$type_resultats == "resultats") {
        
        df_couleurs_candidats <- data.frame("nom_candidat" = data_elections$data[[election_selectionnee()]]$candidatsElection,
                                            "couleur" = data_elections$data[[election_selectionnee()]]$couleursCandidats)
        
        return(merge(donnees_geo_winner, df_couleurs_candidats, by = "nom_candidat"))
        
      } 
      
      return(donnees_geo_winner)
      
    })
    
    output$carto_resultats <- renderLeaflet({
      #####################################
      # création de la palette de couleur #
      ####################################
      req(donnees_carto_vainqueur_by_unite_geo())
      req(donnees_geo_selectionnees())
      
      donnees_geo_winner <-
        donnees_carto_vainqueur_by_unite_geo() %>%
        mutate(pct = pct * 100)
      
      if(input$niveau_geo_restitution == "id_bureau") {
        zone <- paste("BV", donnees_geo_winner$code)
      } else {
        zone <- paste("LV", donnees_geo_winner$libelle)
      }
      
      
      if(input$type_resultats == "resultats") {
        
        popup <-  paste0("<strong>Zone: </strong>",
                         zone,
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
      } else {
        
        pal <- colorNumeric(palette = "YlOrRd", domain = donnees_geo_winner$pct)
        
        popup <-  paste0("<strong>Zone: </strong>",
                         zone,
                         "<br><strong>Abstention (%): </strong>",
                         sprintf("%.2f",donnees_geo_winner$pct))
        
        leaflet(donnees_geo_winner) %>%
          addTiles() %>%
          setView(zoom = 11.5, lat = 44.859684, lng = -0.568365) %>%
          addPolygons(fillColor = ~pal(pct), color = "grey",
                      weight = 1, smoothFactor = 0.5,
                      opacity = 1, fillOpacity = .8,
                      popup = popup,
                      highlightOptions = leaflet::highlightOptions(color = "black", weight = 2,
                                                                   bringToFront = TRUE))
        
      }
      
    })
    
  })
}

## To be copied in the UI
# mod_observer_1_election_resultats_carto_candidat_vainqueur_ui("observer_1_election_resultats_carto_candidat_vainqueur_ui_1")

## To be copied in the server
# mod_observer_1_election_resultats_carto_candidat_vainqueur_server("observer_1_election_resultats_carto_candidat_vainqueur_ui_1")
