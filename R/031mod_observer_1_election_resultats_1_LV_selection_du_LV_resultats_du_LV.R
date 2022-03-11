#' observer_1_election_selection_LV_sur_carte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_observer_1_election_selection_LV_sur_carte_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 7,
             div(class = "container",
                 style = "display:flex;
        flex-direction : column;
        justify-content: space-evenly",
                 
                 div(class ="title_crazy title_container",
                     div(icon(name="democrat", class = "icon_title")),
                     div(h2("Sélection d'un lieu de vote", class = "text-uppercase")),
                     div(icon(name="democrat", class = "icon_title"))
                 ),
                 
                 div(
                   radioButtons(inputId = ns("numero_scrutin"),
                                label = "Choisir un scrutin",
                                choiceNames = "Aucune élection sélectionnée",
                                choiceValues = "",
                                inline = TRUE)
                 ),
                 
                 div(class = "map_container",
                     div(class = "map", id = ns("map"),
                         leafletOutput(ns("myBVmap"), height = 800)
                     ),
                     div(class = "centered", id = ns("message_absence_donnees_carto"),
                         h1("Les données de localisation des bureaux ne sont pas disponibles pour ce scrutin ou cette commune")
                     )
                     
                 )
             )
      ),
      
      column(width = 5,
             div(class = "container",
                 style = "display:flex;
        flex-direction : column;
        justify-content: space-between",
                 
                 div(class ="title_crazy title_container",
                     div(icon(name="democrat", class = "icon_title")),
                     div(h2("Résultats par BV", class = "text-uppercase")),
                     div(icon(name="democrat", class = "icon_title"))
                 ),
                 
                 div(
                   plotOutput(ns("plot_resultats_BV"))
                 ),
                 
                 div(class ="title_crazy title_container",
                     div(icon(name="democrat", class = "icon_title")),
                     div(h2("Résultats par LV", class = "text-uppercase")),
                     div(icon(name="democrat", class = "icon_title"))
                 ),
                 
                 div(
                   plotOutput(ns("plot_resultats_LV"))
                 )
             )
      )
    )
  )
}

### RAJOUTER SELECTION D'UN TOUR

#' observer_1_election_selection_LV_sur_carte Server Functions
#'
#' @noRd 
mod_observer_1_election_selection_LV_sur_carte_server <- function(id, election_selectionnee_d){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
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
      
      election_selectionnee_tour_selectionne_d() %>% 
        filter(id_bureau %in% bv_selectionnes$code)
    })
    
    resultats_by_BV <- reactive({
      
      # req(isTruthy(filtered_data_by_BV()))
      
      compute_resultats_elections(data = filtered_data_by_BV(), 
                                  type = "participation", 
                                  grouping_vars = c(
                                    "nom_election", "type_election", "annee_election", 
                                    "numero_tour", "nom_candidat", "nom", "nom_candidat_short",
                                    "id_bureau"
                                  )
      )
      
    }) %>% debounce(500)
    
    
    resultats_by_LV <- reactive({
      
      # req(isTruthy(filtered_data_by_BV()))
      
      compute_resultats_elections(data = filtered_data_by_BV(), 
                                  type = "participation", 
                                  grouping_vars = c(
                                    "nom_election", "type_election", "annee_election", 
                                    "numero_tour", "nom_candidat", "nom", "nom_candidat_short",
                                    "nom_lieu"
                                  )
      )
      
    })
    
    output$plot_resultats_BV <- renderPlot({
      validate(
        need(!is.null(input$myBVmap_marker_click), "Sélectionnez 1 lieu de vote")
      )
      
      graphique_resultats_election(data = resultats_by_BV(), x = nom_candidat_short, y = pct, 
                                   fill = nom_candidat, 
                                   facet = TRUE, facet_var = id_bureau, 
                                   theme_fun = theme_bdxmetro_dark_mod(regular_font_family = "Nunito",
                                                                       light_font_family = "Nunito",
                                                                       axis.text.x = element_blank()),
                                   title = "", subtitle = "", caption = "", xlab = "", ylab = "Vote (%)", legend_name = "Candidat")
      
    })
    
    
    output$plot_resultats_LV <- renderPlot({
      
      validate(
        need(!is.null(input$myBVmap_marker_click), "Sélectionnez 1 lieu de vote")
        
      )
      
      print(isTruthy(resultats_by_LV()))
      print(nrow(resultats_by_LV()))
      
      graphique_resultats_election(data = resultats_by_LV(), x = nom_candidat_short, y = pct, fill = nom_candidat, 
                                   facet = TRUE, facet_var = nom_lieu, 
                                   theme_fun = theme_bdxmetro_dark_mod(regular_font_family = "Nunito",
                                                                       light_font_family = "Nunito",
                                                                       axis.text.x = element_blank()),
                                   title = "", subtitle = "", caption = "", xlab = "", ylab = "Vote (%)", legend_name = "Candidat")
      
      
    })
    
    
  })
}

## To be copied in the UI
# mod_observer_1_election_selection_LV_sur_carte_ui("observer_1_election_selection_LV_sur_carte_ui_1")

## To be copied in the server
# mod_observer_1_election_selection_LV_sur_carte_server("observer_1_election_selection_LV_sur_carte_ui_1")
