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
             # actionButton(ns("pause"), "Poz"),
             div(class = "container",
                 style = "display:flex;
                          flex-direction : column;
                          justify-content: space-evenly",
                 
                 div(class ="title_section title_container",
                     div(icon(name="democrat", class = "icon_title")),
                     div(h2("S\u00e9lection d\'un lieu de vote", class = "text-uppercase")),
                     div(icon(name="democrat", class = "icon_title"))
                 ),
                 
                 div(
                   radioButtons(inputId = ns("numero_scrutin"),
                                label = "Choisir un scrutin",
                                choiceNames = "Aucune \u00e9lection s\u00e9lectionn\u00e9e",
                                choiceValues = "",
                                inline = TRUE)
                 ),
                 
                 div(class = "map_container",
                     div(class = "map", id = ns("map"),
                         leafletOutput(ns("myBVmap"), height = 800)
                     ),
                     div(class = "centered", id = ns("message_absence_donnees_carto"),
                         h1("Les donn\u00e9es de localisation des bureaux ne sont pas disponibles pour ce scrutin ou cette commune")
                     )
                     
                 )
             )
      ),
      
      column(width = 5,
             div(class = "container",
                 style = "display:flex;
        flex-direction : column;
        justify-content: space-between",
        
        div(class ="title_section title_container",
            div(icon(name="democrat", class = "icon_title")),
            div(h2("R\u00e9sultats par BV", class = "text-uppercase")),
            div(icon(name="democrat", class = "icon_title"))
        ),
        
        div(
          girafeOutput(ns("plot_resultats_BV"))
        ),
        
        div(class ="title_section title_container",
            div(icon(name="democrat", class = "icon_title")),
            div(h2("R\u00e9sultats par LV", class = "text-uppercase")),
            div(icon(name="democrat", class = "icon_title"))
        ),
        
        div(
          girafeOutput(ns("plot_resultats_LV"))
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
mod_observer_1_election_selection_LV_sur_carte_server <- function(id, data_elections, election_selectionnee){
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
    
    # Partie carto
    #### Carte initiale
    myBVmap <- createLeafletMap(session, 'myBVmap')
    
    session$onFlushed(once = T, function() {
      
      output$myBVmap <- renderLeaflet({
        
        req(election_selectionnee())
        req(data_elections$data[[election_selectionnee()]]$cartoLV)
        
        popup_markers <- paste("<b>Lieu de vote</b>", data_elections$data[[election_selectionnee()]]$cartoLV$libelle)
        
        icons <- awesomeIcons(
          icon = 'ios-close',
          iconColor = color_vector(),
          library = 'ion',
          markerColor = color_vector()
        )
        
        secteurs_votes <- data_elections$data[[election_selectionnee()]]$cartoBV %>%
          group_by(libelle, rs_el_lieuvote_p) %>%
          summarise(geometry = st_union(geometry)) %>%
          ungroup()
        
        
        leaflet(data_elections$data[[election_selectionnee()]]$cartoLV) %>% 
          addTiles() %>% 
          setView(zoom = 11.5, lat =44.859684, lng=-0.568365) %>% 
          addAwesomeMarkers(popup = popup_markers, layerId = data_elections$data[[election_selectionnee()]]$cartoLV$gid, icon = icons) %>% 
          addPolygons( data = secteurs_votes,
                       weight = 1, smoothFactor = 0.5,
                       opacity = .75, fillOpacity = .2,
                       highlightOptions = highlightOptions(color = "black", weight = 2,
                                                           bringToFront = TRUE))
        
        
      })
    })
    
    
    
    observeEvent(input$myBVmap_marker_click, { 
      p <- input$myBVmap_marker_click
      print(p)
      
      leafletProxy("myBVmap") %>% 
        setView(zoom = input$myBVmap_zoom, lng = input$myBVmap$lng, lat = input$myBVmap$lat)
      
    })
    
    
    clickedIds <- reactiveValues(ids = vector())
    
    color_vector <- reactive({
      
      if(is.null(input$myBVmap_marker_click)) rep("blue", length(data_elections$data[[election_selectionnee()]]$cartoLV$gid))
      else {
        clicked_marker <- input$myBVmap_marker_click$id 
        
        ifelse(data_elections$data[[election_selectionnee()]]$cartoLV$gid == clicked_marker, "red", "blue")
      }
      
    })
    
    resultats_by_BV <- reactive({
      req(election_selectionnee())
      req(data_elections$data[[election_selectionnee()]]$cartoLV)
      req(input$myBVmap_marker_click)
      
      # on récupère les bv associés au lv sélectionné
      bv_selectionnes <- data_elections$data[[election_selectionnee()]]$cartoBV %>% 
        filter(rs_el_lieuvote_p %in% input$myBVmap_marker_click$id)
        
      data_elections$data[[election_selectionnee()]]$resultatsBV %>%
        filter(id_bureau %in% bv_selectionnes$code) %>% 
        filter(numero_tour %in% input$numero_scrutin) %>% 
        # on filtre pour ne garder que les 8 premiers candidats 
        filter(nom_candidat %in% unique(
          data_elections$data[[election_selectionnee()]]$resultatsGlobauxCommune$nom_candidat
        )
        )
      
    }) %>% debounce(500)
    
    
    resultats_by_LV <- reactive({
      req(election_selectionnee())
      req(data_elections$data[[election_selectionnee()]]$cartoLV)
      req(input$myBVmap_marker_click)
      
      # on récupère les bv associés au lv sélectionné
      bv_selectionnes <- data_elections$data[[election_selectionnee()]]$cartoBV %>% 
        filter(rs_el_lieuvote_p %in% input$myBVmap_marker_click$id)
      
      data_elections$data[[election_selectionnee()]]$resultatsLV %>%
        filter(id_lieu %in% unique(bv_selectionnes$rs_el_lieuvote_p)) %>% 
        filter(numero_tour %in% input$numero_scrutin) %>% 
        # on filtre pour ne garder que les 8 premiers candidats 
        filter(nom_candidat %in% unique(
          data_elections$data[[election_selectionnee()]]$resultatsGlobauxCommune$nom_candidat
        )
        )
      
    })
    
    
    output$plot_resultats_BV <- renderGirafe({
      validate(
        need(!is.null(input$myBVmap_marker_click), "S\u00e9lectionnez 1 lieu de vote")
      )
      
      req(election_selectionnee())
      req(data_elections$data[[election_selectionnee()]]$cartoLV)
      
      g <- graphique_resultats_election(data = arrange(resultats_by_BV(), nom), 
                                   x = nom_candidat_short, y = pct,
                                   fill = nom_candidat,
                                   facet = TRUE, facet_var = id_bureau,
                                   theme_fun = theme_bdxmetro_dark_mod(regular_font_family = "Nunito",
                                                                       light_font_family = "Nunito",
                                                                       axis.text.x = element_blank()),
                                   title = "", subtitle = "", caption = "NB : seuls les 8 premiers candidats sont affich\u00e9s",
                                   xlab = "", ylab = "Vote (%)", legend_name = "Candidat",
                                   scale_fill_function = scale_fill_manual(values = data_elections$data[[election_selectionnee()]]$couleursCandidats,
                                                                           breaks = data_elections$data[[election_selectionnee()]]$candidatsElection))
      
      girafe(
        ggobj = g, 
      )
      
    })
    
    
    output$plot_resultats_LV <- renderGirafe({
      
      validate(
        need(!is.null(input$myBVmap_marker_click), "S\u00e9lectionnez 1 lieu de vote")
      )
      
      req(election_selectionnee())
      req(data_elections$data[[election_selectionnee()]]$cartoLV)
      
      g <- graphique_resultats_election(data = arrange(resultats_by_LV(), nom),
                                   x = nom_candidat_short, y = pct, fill = nom_candidat,
                                   facet = TRUE, facet_var = nom_lieu,
                                   theme_fun = theme_bdxmetro_dark_mod(regular_font_family = "Nunito",
                                                                       light_font_family = "Nunito",
                                                                       axis.text.x = element_blank()),
                                   title = "", subtitle = "", caption = "NB : seuls les 8 premiers candidats sont affich\u00e9s",
                                   xlab = "", ylab = "Vote (%)", legend_name = "Candidat",
                                   scale_fill_function = scale_fill_manual(values = data_elections$data[[election_selectionnee()]]$couleursCandidats,
                                                                           breaks = data_elections$data[[election_selectionnee()]]$candidatsElection))
      
      girafe(
        ggobj = g, 
      )
      
      
    })
    
    
  })
}

## To be copied in the UI
# mod_observer_1_election_selection_LV_sur_carte_ui("observer_1_election_selection_LV_sur_carte_ui_1")

## To be copied in the server
# mod_observer_1_election_selection_LV_sur_carte_server("observer_1_election_selection_LV_sur_carte_ui_1")
