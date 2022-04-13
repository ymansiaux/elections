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
#' @importFrom bdxmetroidentity scale_fill_bdxmetro_discrete theme_bdxmetro_dark
#' @importFrom ggtext element_markdown
#' @importFrom viridis scale_fill_viridis
#' @importFrom stringr str_trim str_replace
#' @importFrom leaflet leafletOutput createLeafletMap renderLeaflet awesomeIcons leaflet addTiles setView addAwesomeMarkers addPolygons highlightOptions leafletProxy setView colorNumeric addLegend

mod_observer_1_election_resultats_globaux_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # actionButton(ns("pause2"), "pause2"),
      column(width = 12,
             mod_filter_donnees_observer_1_election_ui(ns("observer_1_election_resultats_globaux_ui_1"))
      )
    ),
    
    fluidRow(
      column(width = 7,
             mod_observer_1_election_resultats_globaux_carto_ui(ns("observer_1_election_resultats_globaux_carto_ui_1"))
             
      ),
      column(width = 5,
             mod_observer_1_election_resultats_globaux_barplot_ui(ns("observer_1_election_resultats_globaux_barplot_ui_1")))
      
    )
    
  )
}

#' observer_1_election Server Functions
#'
#' @noRd 
mod_observer_1_election_resultats_globaux_server <- function(id, data_elections, debug_whereami){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv <- reactiveValues(name_election = NULL)
    
    mod_filter_donnees_observer_1_election_server("observer_1_election_resultats_globaux_ui_1", data_elections, rv)
    
    
    ## SOUS MODULES ( COMPARER AVEC LES NS )
    mod_observer_1_election_resultats_globaux_barplot_server("observer_1_election_resultats_globaux_barplot_ui_1", 
                                                             data_elections, 
                                                             election_selectionnee = reactive(rv$name_election)
    )
    
    mod_observer_1_election_resultats_globaux_carto_server("observer_1_election_resultats_globaux_carto_ui_1", 
                                                           data_elections, 
                                                           election_selectionnee = reactive(rv$name_election)
    )
    # mod_observer_1_election_resultats_globaux_barplot_server("observer_1_election_resultats_globaux_ui_1", election_selectionnee_d)
    # mod_observer_1_election_resultats_globaux_carto_server("observer_1_election_resultats_globaux_ui_1", election_selectionnee_d)
  })
}

## To be copied in the UI
# mod_observer_1_election_ui("observer_1_election_ui_1")

## To be copied in the server
# mod_observer_1_election_server("observer_1_election_ui_1")
