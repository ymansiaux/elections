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
      column(width = 10,
             
             div(class ="title_crazy title_container",
                 div(icon(name="democrat", class = "icon_title")),
                 div(h1("Résultats globaux", class = "text-uppercase title")),
                 div(icon(name="democrat", class = "icon_title"))
             )
      )
    ),
    
    fluidRow(
      column(width = 10,
             
             plotOutput(ns("graphique_resultats"))
      )
    ),
    
    fluidRow(
      column(width = 10,
             div(class ="title_crazy title_container",
                 div(icon(name="democrat", class = "icon_title")),
                 div(h1("Abstention", class = "text-uppercase title")),
                 div(icon(name="democrat", class = "icon_title"))
             )
      )
    ),
    
    fluidRow(
      column(width = 10,
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
      req(election_selectionnee_d())
      
      compute_resultats_elections(data = election_selectionnee_d(), 
                                  type = "participation", 
                                  grouping_vars = c(
                                    "nom_election", "type_election", "annee_election", 
                                    "numero_tour", "nom_candidat", "nom", "nom_candidat_short")) %>%
        graphique_resultats_election(data = ., x = nom_candidat_short, y = pct, fill = nom_candidat, facet = TRUE, facet_var = numero_tour)
      
    })
    
    
    output$graphique_abstention <- renderPlot({
      
      compute_resultats_elections(data = election_selectionnee_d(),
                                  type = "abstention",
                                  grouping_vars = c(
                                    "nom_election", "type_election", "annee_election", "numero_tour")) %>% 
        graphique_resultats_election(data = ., x = numero_tour, y = pct, fill = numero_tour, facet = FALSE)
      
    })
    
  })
}

## To be copied in the UI
# mod_observer_1_election_resultats_globaux_ui("observer_1_election_resultats_globaux_ui_1")

## To be copied in the server
# mod_observer_1_election_resultats_globaux_server("observer_1_election_resultats_globaux_ui_1")
