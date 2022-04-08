#' observer_1_election_resultats_globaux UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggsci pal_npg
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
mod_observer_1_election_resultats_globaux_barplot_server <- function(id, data_elections, election_selectionnee){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    observe(print(election_selectionnee())
    )
    
    output$graphique_resultats <- renderPlot({
      req(election_selectionnee())
      
      graphique_resultats_election(data = arrange(data_elections$data[[election_selectionnee()]]$resultatsGlobauxCommune, nom),
                                   x = nom_candidat_short, y = pct, fill = nom_candidat,
                                   facet = TRUE, facet_var = numero_tour,
                                   theme_fun = theme_bdxmetro_dark_mod(regular_font_family = "Nunito",
                                                                       light_font_family = "Nunito",
                                                                       axis.text.x = element_blank()),
                                   title = "", subtitle = "", caption = "NB : seuls les 8 premiers candidats sont affichés",
                                   xlab = "", ylab = "Voix (%)", legend_name = "Candidat",
                                   scale_fill_function = scale_fill_manual(values = data_elections$data[[election_selectionnee()]]$couleursCandidats,
                                                                           breaks = data_elections$data[[election_selectionnee()]]$candidatsElection)
      )
      
    })
    # 
    # 
    output$graphique_abstention <- renderPlot({
      
      req(election_selectionnee())
      
      
      graphique_resultats_election(data = data_elections$data[[election_selectionnee()]]$resultatsAbstention,
                                   x = numero_tour, y = pct, fill = numero_tour,
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
