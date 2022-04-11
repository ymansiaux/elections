#' observer_1_election_resultats_selectionLVBV UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_observer_1_election_resultats_1candidat_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             mod_filter_donnees_observer_1_election_ui(ns("observer_1_election_resultats_1candidat_ui_1"))
      )
    ),
    # 
    fluidRow(
      column(width = 12,
             mod_observer_1_election_resultats_1candidat_selection_candidat_ui(ns("observer_1_election_resultats_selection_candidat_specifique_ui_1"))
             
      )
    )
    
  )
}

#' observer_1_election_resultats_selectionLVBV Server Functions
#'
#' @noRd 
mod_observer_1_election_resultats_1candidat_server <- function(id, data_elections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(name_election = NULL)
    
    mod_filter_donnees_observer_1_election_server("observer_1_election_resultats_1candidat_ui_1", data_elections, rv)
    
    mod_observer_1_election_resultats_1candidat_selection_candidat_server("observer_1_election_resultats_selection_candidat_specifique_ui_1",  
                                                          data_elections, 
                                                          election_selectionnee = reactive(rv$name_election))
  })
}
## To be copied in the UI
# mod_observer_1_election_resultats_selectionLVBV_ui("observer_1_election_resultats_selectionLVBV_ui_1")

## To be copied in the server
# mod_observer_1_election_resultats_selectionLVBV_server("observer_1_election_resultats_selectionLVBV_ui_1")
