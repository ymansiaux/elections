#' observer_1_election_resultats_globaux UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_observer_1_election_resultats_globaux_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             plotOutput(ns("graphique_resultats")),
             plotOutput(ns("graphique_abstention"))
      )
    )
  )
}

#' observer_1_election_resultats_globaux Server Functions
#'
#' @noRd 
mod_observer_1_election_resultats_globaux_server <- function(id, election_selectionnee_d){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    
    output$graphique_resultats <- renderPlot({
      compute_resultats_elections(data = election_selectionnee_d(), 
                                  type = "participation", 
                                  grouping_vars = c(
                                    "nom_election", "type_election", "annee_election", 
                                    "numero_tour", "nom_candidat", "nom", "nom_candidat_short")) %>%
        graphique_resultats_election(data = ., x = nom_candidat_short, y = pct, fill = nom_candidat)
      
    })
    
    
    output$graphique_abstention <- renderPlot({
      
      compute_resultats_elections(data = election_selectionnee_d(),
                                  type = "abstention",
                                  grouping_vars = c(
                                    "nom_election", "type_election", "annee_election", "numero_tour")) %>% 
        graphique_resultats_election(data = ., x = numero_tour, y = pct, fill = numero_tour)
      
    })
    
  })
}

## To be copied in the UI
# mod_observer_1_election_resultats_globaux_ui("observer_1_election_resultats_globaux_ui_1")

## To be copied in the server
# mod_observer_1_election_resultats_globaux_server("observer_1_election_resultats_globaux_ui_1")
