#' observer_1_election_resultats_globaux UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_observer_1_election_resultats_globaux_barplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      actionButton(ns("pause"), "Pause"),
      column(width = 12,
             div(class = "container",
                 style = "display:flex;
                          flex-direction : column;
                          justify-content: space-between",
                 
                 div(class ="title_section title_container",
                     div(icon(name="democrat", class = "icon_title")),
                     div(h2("Résultats par candidats", class = "text-uppercase")),
                     div(icon(name="democrat", class = "icon_title"))
                 ),
                 
                 div(
                   plotOutput(ns("graphique_resultats"))
                 ),
                 
                 div(class ="title_section title_container",
                     div(icon(name="democrat", class = "icon_title")),
                     div(h2("Abstention", class = "text-uppercase")),
                     div(icon(name="democrat", class = "icon_title"))
                 ),
                 
                 div(
                   plotOutput(ns("graphique_abstention"))
                 )
             )
      )
    )
  )
  
}

#' observer_1_election_resultats_globaux Server Functions
#'
#' @noRd 
mod_observer_1_election_resultats_globaux_barplot_server <- function(id, election_selectionnee_d){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    
    observeEvent(input$pause, browser())
    
    output$graphique_resultats <- renderPlot({
      req(election_selectionnee_d())
      
      compute_resultats_elections(data = election_selectionnee_d(), 
                                  type = "participation", 
                                  grouping_vars = c(
                                    "nom_election", "type_election", "annee_election", 
                                    "numero_tour", "nom_candidat", "nom", "nom_candidat_short"
                                  ),
                                  truncate_results = TRUE, n_first_results = 8
                                  ) %>%
        
        graphique_resultats_election(data = ., x = nom_candidat_short, y = pct, fill = nom_candidat, 
                                     facet = TRUE, facet_var = numero_tour, 
                                     theme_fun = theme_bdxmetro_dark_mod(regular_font_family = "Nunito",
                                                                         light_font_family = "Nunito",
                                                                         axis.text.x = element_blank()),
                                     title = "", subtitle = "", caption = "NB : seuls les 8 premiers candidats sont affichés", xlab = "", ylab = "Voix (%)", legend_name = "Candidat")
      
    })
    
    
    output$graphique_abstention <- renderPlot({
      
      
      compute_resultats_elections(data = election_selectionnee_d(),
                                  type = "abstention",
                                  grouping_vars = c(
                                    "nom_election", "type_election", "annee_election", "numero_tour")) %>% 
        
        graphique_resultats_election(data = ., x = numero_tour, y = pct, fill = numero_tour, 
                                     facet = FALSE, 
                                     theme_fun = theme_bdxmetro_dark_mod(regular_font_family = "Nunito",
                                                                         light_font_family = "Nunito",
                                                                         axis.text.x = element_blank()),
                                     title = "", subtitle = "", caption = "", 
                                     xlab = "", ylab = "Abstention (%)", legend_name = "Tour",
                                     scale_fill_function = scale_color_discrete_c4a_cat(palette = "harmonic"))
      
    })
    
  })
}

## To be copied in the UI
# mod_observer_1_election_resultats_globaux_ui("observer_1_election_resultats_globaux_ui_1")

## To be copied in the server
# mod_observer_1_election_resultats_globaux_server("observer_1_election_resultats_globaux_ui_1")
