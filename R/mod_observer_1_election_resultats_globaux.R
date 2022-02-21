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
      column(width = 2,
             ## comparer avec shinydashboard box et bs4dash bs4TabCard {bs4Dash}
             tags$div(
               class = "nav flex-column nav-pills",
               style = "margin-top:100px;",
               id = "v-pills-tab",
               role = "tablist",
               `aria-orientation` = "vertical",
               tags$a(
                 class = "nav-link active",
                 id = "v-pills-home-tab",
                 `data-toggle` = "pill",
                 href = "#v-pills-home",
                 role = "tab",
                 `aria-controls` = "v-pills-home",
                 `aria-selected` = "true",
                 "Home"
               ),
               tags$a(
                 class = "nav-link",
                 id = "v-pills-profile-tab",
                 `data-toggle` = "pill",
                 href = "#v-pills-profile",
                 role = "tab",
                 `aria-controls` = "v-pills-profile",
                 `aria-selected` = "false",
                 "Profile"
               ),
               tags$a(
                 class = "nav-link",
                 id = "v-pills-messages-tab",
                 `data-toggle` = "pill",
                 href = "#v-pills-messages",
                 role = "tab",
                 `aria-controls` = "v-pills-messages",
                 `aria-selected` = "false",
                 "Messages"
               ),
               tags$a(
                 class = "nav-link",
                 id = "v-pills-settings-tab",
                 `data-toggle` = "pill",
                 href = "#v-pills-settings",
                 role = "tab",
                 `aria-controls` = "v-pills-settings",
                 `aria-selected` = "false",
                 "Settings"
               )
             )
             ),
      column(width = 10,
             tags$div(
               class = "tab-content",
               id = "v-pills-tabContent",
               tags$div(
                 class = "tab-pane fade show active",
                 id = "v-pills-home",
                 role = "tabpanel",
                 `aria-labelledby` = "v-pills-home-tab",
                 plotOutput(ns("graphique_resultats"))
               ),
               tags$div(
                 class = "tab-pane fade",
                 id = "v-pills-profile",
                 role = "tabpanel",
                 `aria-labelledby` = "v-pills-profile-tab",
                 plotOutput(ns("graphique_abstention"))
               ),
               tags$div(
                 class = "tab-pane fade",
                 id = "v-pills-messages",
                 role = "tabpanel",
                 `aria-labelledby` = "v-pills-messages-tab",
                 "..."
               ),
               tags$div(
                 class = "tab-pane fade",
                 id = "v-pills-settings",
                 role = "tabpanel",
                 `aria-labelledby` = "v-pills-settings-tab",
                 "..."
               )
             )
      )
    ),
    
    fluidRow(
      column(width = 12,
             
             
             h2("toto")
             
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
