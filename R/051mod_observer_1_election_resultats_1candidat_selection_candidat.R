#' observer_1_election_selection_1_candidat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_observer_1_election_resultats_1candidat_selection_candidat_ui <- function(id){
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
                     div(h2("Sélection d'un candidat", class = "text-uppercase")),
                     div(icon(name="democrat", class = "icon_title"))
                 ),
                 
                 div(class = "container",
                     style = "display:flex;
                             flex-direction : row;
                             justify-content: space-around",
                     div(   
                       radioButtons(inputId = ns("numero_scrutin"),
                                    label = "Choisir un scrutin",
                                    choiceNames = "Aucune élection sélectionnée",
                                    choiceValues = "",
                                    inline = TRUE)
                     ),
                     div(
                       selectizeInput(
                         inputId = ns("candidat"),
                         label = "Sélectionner un candidat",
                         choices = NULL,
                         multiple = FALSE,
                         options = list(deselectBehavior = "top")
                       )
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
                 ),
                 
                 div(class = "map_container",
                     div(class = "map", id = ns("map"),
                         leafletOutput(ns("carto_resultats"), height = 800)
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
                 
                 # div(class ="title_section title_container",
                 #     div(icon(name="democrat", class = "icon_title")),
                 #     div(h2("Résultats par BV", class = "text-uppercase")),
                 #     div(icon(name="democrat", class = "icon_title"))
                 # ),
                 # 
                 # div(
                 #   plotOutput(ns("barplot_BV"))
                 # ),
                 
                 div(class ="title_section title_container",
                     div(icon(name="democrat", class = "icon_title")),
                     div(h2("Résultats par LV", class = "text-uppercase")),
                     div(icon(name="democrat", class = "icon_title"))
                 ),
                 
                 div(
                   girafeOutput(ns("barplot_LV"), height = "600px")
                 )
             )
      )
      
    )
    
  )
}

#' observer_1_election_selection_1_candidat Server Functions
#'
#' @noRd 
mod_observer_1_election_resultats_1candidat_selection_candidat_server <- function(id, data_elections, 
                                                                                  election_selectionnee){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$pause, browser())
    
    
    observe({
      
      if(is.null(election_selectionnee()))  {
        
        runjs(glue('$("#{ns("message_absence_donnees_carto")}").hide();'));
 
      } else {
        
        updateSelectizeInput(session,
                             inputId = "candidat",
                             choices = sort(unique(data_elections$data[[election_selectionnee()]]$donneesElection$nom_candidat)),
                             selected = sort(unique(data_elections$data[[election_selectionnee()]]$donneesElection$nom_candidat))[1],
                             server = TRUE
        )

        
        if(!data_elections$data[[election_selectionnee()]]$donneesElection$annee_election[1] %in% annees_elections_avec_donnees_geo | 
           !data_elections$data[[election_selectionnee()]]$donneesElection$code_insee[1] %in% communes_elections_avec_donnees_geo) {
          
          runjs(glue('$("#{ns("map")}").addClass("map_with_opacity");'));
          runjs(glue('$("#{ns("message_absence_donnees_carto")}").show();'));
          
        } else {
          
          runjs(glue('$("#{ns("map")}").removeClass("map_with_opacity");'));
          runjs(glue('$("#{ns("message_absence_donnees_carto")}").hide();'));
          
        }
      }
    })

    observeEvent(input$candidat, {
      #whereami::cat_where(where = whereami::whereami())
      
      if(input$candidat %in% data_elections$data[[election_selectionnee()]]$donneesElection$nom_candidat) {
        
        tours_dispos <- data_elections$data[[election_selectionnee()]]$donneesElection %>% 
          filter(nom_candidat %in% input$candidat) %>% 
          select(numero_tour) %>% 
          distinct() %>% 
          pull()
        
        
        updateRadioButtons(session,
                           inputId = "numero_scrutin",
                           choiceNames = paste("Tour", sort(tours_dispos)),
                           choiceValues = sort(tours_dispos)
        )
      }
    })
    

    resultats_elections_candidat <- reactive({

      req(input$candidat %in% data_elections$data[[election_selectionnee()]]$donneesElection$nom_candidat)
      req(input$candidat)
      req(input$numero_scrutin)

      list("resultats_BV" = data_elections$data[[election_selectionnee()]]$resultatsBV %>%
             filter(nom_candidat %in% input$candidat & numero_tour %in% input$numero_scrutin),

           "resultats_LV" = data_elections$data[[election_selectionnee()]]$resultatsLV %>%
             filter(nom_candidat %in% input$candidat & numero_tour %in% input$numero_scrutin)



      )

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
              resultats_elections_candidat()$resultats_BV,
              by.x = "code", by.y = input$niveau_geo_restitution)

      } else {

        merge(donnees_geo_selectionnees(),
              resultats_elections_candidat()$resultats_LV,
              by.x = "rs_el_lieuvote_p", by.y = input$niveau_geo_restitution)

      }

    })
    
    output$carto_resultats <- renderLeaflet({

      validate(
        need(!is.null(election_selectionnee()), "Sélectionnez 1 élection")
      )
      
      donnees_carte <- donnees_cartos() %>%
        mutate(pct = pct * 100)

      popup <-  paste0("<strong>Zone: </strong>",
                       donnees_carte$libelle,
                       "<br><strong>Candidat: </strong>",
                       donnees_carte$nom_candidat,
                       "<br><strong>% recueillis: </strong>",
                       sprintf("%.2f",donnees_carte$pct))
     
     pal <- colorNumeric(palette = "YlOrRd", domain = donnees_carte$pct)
   
      leaflet(donnees_carte) %>%
        addTiles() %>%
        setView(zoom = 11.5, lat = 44.859684, lng = -0.568365) %>%
        addPolygons(fillColor = ~pal(pct), color = "grey",
                    weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = .8,
                    popup = popup,
                    highlightOptions = leaflet::highlightOptions(color = "black", weight = 2,
                                                                 bringToFront = TRUE)) %>%
        addLegend(pal = pal, values = ~pct, group = "circles", position = "topright")
    })

    output$barplot_LV <- renderGirafe({
      validate(
        need(!is.null(election_selectionnee()), "Sélectionnez 1 élection")
      )
      g <- graphique_resultats_election(data = resultats_elections_candidat()$resultats_LV,
                                   x = nom_lieu, y = pct, fill = nom_lieu,
                                   facet = FALSE,
                                   theme_fun = theme_bdxmetro_dark_mod(regular_font_family = "Nunito",
                                                                       light_font_family = "Nunito",
                                                                       axis.text.x = element_blank(),
                                                                       legend.position = "none",
                                                                       axis_title_size = 15,
                                                                       axis_text_size = 13,
                                                                       legend_text_size = 10),
                                   title = "", subtitle = "", caption = "Passer la souris sur le graphe pour avoir les valeurs", 
                                   xlab = "", ylab = "Vote (%)", legend_name = "LV",
                                   scale_fill_function = scale_color_discrete_c4a_cat(palette = "harmonic"))
      
      girafe(
        ggobj = g, 
      )

    })
    
    
  })
}

## To be copied in the UI
# mod_observer_1_election_selection_1_candidat_ui("observer_1_election_selection_1_candidat_ui_1")

## To be copied in the server
# mod_observer_1_election_selection_1_candidat_server("observer_1_election_selection_1_candidat_ui_1")
