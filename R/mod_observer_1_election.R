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
#' @importFrom bdxmetroidentity scale_fill_bdxmetro_discrete
#' @importFrom ggtext element_markdown
#' @importFrom viridis scale_fill_viridis
#' @importFrom stringr str_trim str_replace
mod_observer_1_election_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    actionButton(ns("pause"), "pause"),
    
    selectizeInput(
      inputId = ns("type_elections"),
      label = "Type d'election",
      choices = NULL,
      multiple = FALSE,
      options = list(deselectBehavior = "top")
    ),
    
    selectizeInput(
      inputId = ns("annee_elections"),
      label = "Annee de l'election",
      choices = NULL,
      multiple = FALSE,
      options = list(deselectBehavior = "top")
    ),
    plotOutput(ns("plot1"))
    # radioButtons(inputId = ns("tour_election
    
  )
}

#' observer_1_election Server Functions
#'
#' @noRd 
mod_observer_1_election_server <- function(id, data_elections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$pause, browser())
    
    ######
    type_elections <- reactive({
      req(data_elections$data)
      sort(
        unique(
          data_elections$data$type_election
        )
      )
    }) #%>% debounce(100)
    
    annee_elections <- reactive({
      req(data_elections$data)
      req(input$type_elections)
      
      sort(
        unique(
          unlist(
            data_elections$data[type_election %in% input$type_elections, "annee_election"])
        )
      )
    })# %>% debounce(1000)
    
    
    observe({
      updateSelectizeInput(session,
                           inputId = "type_elections",
                           choices = type_elections(),
                           server = TRUE
      )
    })
    
    observeEvent(input$type_elections, {
      updateSelectizeInput(session,
                           inputId = "annee_elections",
                           choices = annee_elections(),
                           server = TRUE
      )
    })
    
    election_selectionnee <- reactive({
      
      req(input$annee_elections)
      req(input$type_elections)
      
      copy(data_elections$data[annee_election %in% input$annee_elections & type_election %in% input$type_elections]) %>% 
        .[is.na(numero_tour), numero_tour := 1]
      
    }) 
    
    results_by_tour_by_candidate <- reactive({
      
      req(isTruthy(election_selectionnee()))
      
      copy(election_selectionnee()) %>% 
        .[, .(
          nb_voix = sum(nb_voix, na.rm = TRUE),
          nb_expr = sum(nb_expr, na.rm = TRUE)
        ),
        by = list(nom_election, type_election, annee_election, numero_tour, nom_candidat)
        ] %>% 
        .[, pct := nb_voix / nb_expr] %>% 
        .[, prenom := get_first_name(nom_candidat)] %>% 
        .[, nom := get_last_name(nom_candidat, prenom)] %>% 
        .[nom == "", nom := prenom]
      
    }) %>% debounce(500)
    
    output$plot1 <- renderPlot({
      # pivoter les axes des x
      # faire un joli theme
      # changer les couleurs
      # interactif
      
      g <- results_by_tour_by_candidate() %>%
        ggplot(aes(x = as.factor(nom), y = pct, fill = as.factor(nom_candidat))) +
        geom_col() +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_viridis(discrete = TRUE) +
        create_theme()
      
      if(any(!is.na(results_by_tour_by_candidate()$numero_tour))) {
        g <- g + facet_wrap(vars(numero_tour), scales = "free") 
      }
      
      g
      
    })
    
    # graphe absention
    abstention <- reactive({
      
      req(isTruthy(election_selectionnee()))
      
      copy(election_selectionnee()) %>% 
        .[, .(
          nb_inscrits = sum(nb_inscrits, na.rm = TRUE),
          nb_votants = sum(nb_votants, na.rm = TRUE)
        ),
        by = list(nom_election, type_election, annee_election, numero_tour)
        ] %>% 
        .[, pct := 1- nb_votants / nb_inscrits] 
    })
    
    output$plot2 <- renderPlot({
      abstention() %>% 
        ggplot(aes(x = as.factor(numero_tour), y = pct)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_viridis(discrete = TRUE) +
        create_theme()
    })
    
    # reprendre les graphes proposés dans l'appli 1
    
    # résultats / BV
    
    # évolution d'un candidat
    
    # graphe vagues avec l'abstention
  })
}

## To be copied in the UI
# mod_observer_1_election_ui("observer_1_election_ui_1")

## To be copied in the server
# mod_observer_1_election_server("observer_1_election_ui_1")
