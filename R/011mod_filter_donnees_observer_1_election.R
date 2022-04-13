#' filter_donnees_observer_1_election UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filter_donnees_observer_1_election_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             div(class = "container",
                 style = "display:flex;
                  flex-direction : row;
                  justify-content: space-evenly",
                 
                 actionButton(ns("download_data"), "Telecharger"),
                 
                 div(
                   selectizeInput(
                     inputId = ns("type_elections"),
                     label = "Type d'election",
                     choices = NULL,
                     multiple = FALSE,
                     options = list(deselectBehavior = "top")
                   )
                 ),
                 div(
                   selectizeInput(
                     inputId = ns("annee_elections"),
                     label = "Annee de l'election",
                     choices = NULL,
                     multiple = FALSE,
                     options = list(deselectBehavior = "top")
                   )
                 ),
                 div(
                   selectizeInput(
                     inputId = ns("commune_elections"),
                     label = "Commune",
                     choices = NULL,
                     multiple = FALSE,
                     options = list(deselectBehavior = "top")
                   ),
                   
                 )
                 
             )
      )
    )
    
    
  )
}

#' filter_donnees_observer_1_election Server Functions
#'
#' @noRd 
mod_filter_donnees_observer_1_election_server <- function(id, data_elections, rv){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    
    type_elections <- reactive({
      req(data_elections$data)
      sort(
        unique(
          data_elections$elections_dispo$type_election
        )
      )
    })
    
    annee_elections <- reactive({
      req(data_elections$data)
      req(input$type_elections)
      
      data_elections$elections_dispo %>%
        filter(type_election %in% input$type_elections) %>% 
        select(annee_election) %>% 
        distinct %>%
        arrange(annee_election) %>% 
        pull()
      
    })
    
    
    commune_elections <- reactive({
      req(data_elections$data)
      req(input$type_elections)
      req(input$annee_elections)
      
      data_elections$elections_dispo %>%
        filter(type_election %in% input$type_elections & annee_election %in% input$annee_elections) %>% 
        select(insee) %>% 
        distinct %>%
        arrange(insee) %>% 
        pull()
      
    })
    
    
    observe({
      updateSelectizeInput(session,
                           inputId = "type_elections",
                           choices = type_elections(),
                           selected = "Municipale",
                           server = TRUE
      )
    })
    
    observeEvent(input$type_elections, {
      updateSelectizeInput(session,
                           inputId = "annee_elections",
                           choices = annee_elections(),
                           selected = 2020,
                           server = TRUE
      )
      
    })
    
    observeEvent(list(input$type_elections, input$annee_elections), {
      updateSelectizeInput(session,
                           inputId = "commune_elections",
                           choices = commune_elections(),
                           selected = 33063,
                           server = TRUE
      )
    })
    
    
    # observeEvent(input$download_data, browser())
    
    # observe(print(data_elections$elections_dispo))
    
    # election_selectionnee <- reactive({
  
    observeEvent(input$download_data, {
      
      req(input$annee_elections)
      req(input$type_elections)
      req(input$commune_elections)
      
      # 
      # input$download_data
      
      name_election <- paste(input$type_elections, input$annee_elections, input$commune_elections, sep = "_")
      
      # appel xtradata pour télécharger les données
      
      if(is.null(data_elections$data[[name_election]]$donneesElection)) {
        # browser()
        # Telechargement des données
        data_elections$data[[name_election]]$download_data()
        
        # Calcul des résultats globaux à la commune
        data_elections$data[[name_election]]$compute_resultats_globaux_commune(data_elections$data[[name_election]]$donneesElection)
        
        # Calcul de l'abstention
        data_elections$data[[name_election]]$compute_abstention_commune(data_elections$data[[name_election]]$donneesElection)
        
        # Calcul des candidats restants (les 8 premiers) et attribution de leur couleur
        data_elections$data[[name_election]]$candidatsElection <- sort(unique(data_elections$data[[name_election]]$resultatsGlobauxCommune$nom_candidat))
        data_elections$data[[name_election]]$couleursCandidats <- pal_npg("nrc")(length(data_elections$data[[name_election]]$candidatsElection))
        
        # Calcul des résultats / BV
        data_elections$data[[name_election]]$compute_resultats_par_BV(data_elections$data[[name_election]]$donneesElection)
        
        # Calcul des résultats / LV
        data_elections$data[[name_election]]$compute_resultats_par_LV(data_elections$data[[name_election]]$donneesElection)
        
        # Calcul de l'abstention / BV
        data_elections$data[[name_election]]$compute_abstention_par_BV(data_elections$data[[name_election]]$donneesElection)
        
        # Calcul de l'abstention / LV
        data_elections$data[[name_election]]$compute_abstention_par_LV(data_elections$data[[name_election]]$donneesElection)
        
        
        # browser()
        # download <- try(xtradata_requete_features(
        #   key = Sys.getenv("XTRADATA_KEY"),
        #   typename = "EL_RESULTAT_A",
        #   filter = data_elections$data[[name_election]]$xtradataParameters,
        #   showURL = TRUE
        # ))
        # 
        # if (inherits(download, "try-error")) {
        #   data_elections$data[[name_election]]$donneesElection <- NULL
        # } else {
        #   data_elections$data[[name_election]]$donneesElection <- download %>% 
        #     mutate(
        #       date_election = as_datetime(date_evenement, tz = "Europe/Paris"),
        #       annee_election = year(date_election)) %>%
        #     mutate(prenom = get_first_name(nom_candidat)) %>%
        #     mutate(nom = get_last_name(nom_candidat, prenom)) %>%
        #     mutate(nom = ifelse(nom == "", prenom, nom)) %>%
        #     mutate(nom_candidat_short = str_sub(nom_candidat,1,10)) %>%
        #     mutate(across(starts_with("nb_"), as.numeric)) %>%
        #     select(-date_evenement, -cdate, -mdate) %>%
        #     rename(nb_voix = valeur, code_insee = insee) %>%
        #     clean_names()
        # }
        
        # return(data_elections$data[[name_election]])
      }
      # print(name_election)
      rv$name_election <- name_election
      # print(rv$name_election)
      return(rv)
      
      
    })
  })
}

## To be copied in the UI
# mod_filter_donnees_observer_1_election_ui("filter_donnees_observer_1_election_ui_1")

## To be copied in the server
# mod_filter_donnees_observer_1_election_server("filter_donnees_observer_1_election_ui_1")
