#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr mutate select filter group_by arrange distinct as_tibble summarise mutate_at vars ungroup inner_join rename slice_max
#' @import sf
#' @importFrom shinyYM closeWaiter add_notie_alert
#' @importFrom xtradata xtradata_requete_features
#' @importFrom stringr str_extract str_sub
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate as_date year as_datetime
#' @importFrom janitor clean_names
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom shinyjs runjs
#' @importFrom cols4all scale_color_discrete_c4a_cat
#' @importFrom purrr pmap
#' @importFrom glue glue glue_data
#' @importFrom rlang as_string ensym
#' @importFrom ggiraph geom_col_interactive girafeOutput renderGirafe girafe


#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  options(bitmapType = "cairo")
  
  observe(closeWaiter(golem::app_prod(), 3))
  
  debug_whereami <- TRUE
  light_dataset <- FALSE
  
  
  mod_accueil_server("accueil_ui_1")
  
  mod_observer_1_election_resultats_globaux_server("observer_1_election_ui_1", data_elections = data_elections, debug_whereami = debug_whereami)
  
  mod_observer_1_election_resultats_selectionLVBV_server("observer_1_election_selection_LV_sur_carte_ui_1", data_elections = data_elections)
  
  mod_observer_1_election_resultats_1candidat_server("observer_1_candidat_ui_1", data_elections = data_elections)
  
  font_add_google(name = "Nunito", family = "Nunito")
  showtext_auto()
  
  
  #######################################
  # Chargement des données au démarrage #
  #######################################
  data_elections <- reactiveValues(data = NULL, elections_dispo = NULL, bureaux_vote = NULL, lieux_vote = NULL)
  
  observeEvent(NULL, ignoreNULL = FALSE, ignoreInit = FALSE, once = TRUE, {
    
    runjs('$(".nav-link").addClass("disabled");');
    
    add_notie_alert(type = "info", text = "Récupération des données ... Patience ...",
                    stay = FALSE, time = 10, position = "top", session)
    
    
    if(light_dataset) {
      dat <-
        try(xtradata_requete_features(key = Sys.getenv("XTRADATA_KEY"), typename = "EL_SYNTHESE_RESULTAT_A",
                                      filter = list(type_election = "Présidentielle"),
                                      showURL = TRUE))
    } else {
      dat <- 
        try(xtradata_requete_features(key = Sys.getenv("XTRADATA_KEY"), typename = "EL_SYNTHESE_RESULTAT_A",
                                      showURL = TRUE))
    }
    # print(dat)
    
    if(inherits(dat, "try-error")) {
      
      add_notie_alert(type = "error", text = "Echec de récupération des données",
                      stay = FALSE, time = 5, position = "bottom", session)
      
      
    } else {
      
      add_notie_alert(type = "success", text = "Connexion à la base OK",
                      stay = FALSE, time = 5, position = "bottom", session)
      
      
      elections_distinctes <- dat %>% 
        mutate(annee_election = year(as_datetime(date_evenement))) %>% 
        select(type_election, annee_election, insee) %>% 
        distinct()  
      
      data_elections$data <- pmap(elections_distinctes, ~ createR6Election(dat, ..1, ..2, ..3))
      
      names_elections <- elections_distinctes %>% 
        mutate(name_election = paste(type_election, annee_election, insee, sep = "_"))
      
      names(data_elections$data) <- names_elections$name_election
      
      data_elections$elections_dispo <- names_elections
      
      runjs('$(".nav-link").removeClass("disabled");');
      
    }
  })
  
  
  ### PARTIE BDXMETROIDENTITY ###
  rv <- reactiveValues()
  rv$theme <- "dark"
  
  output$my_logo <- renderUI({
    if (rv$theme == "light") {
      tags$img(src = "www/datalab-logo-lightmode.png", width = "150px")
    } else if (rv$theme == "dark") {
      tags$img(src = "www/datalab-logo-darkmode.png", width = "150px")
    }
  })
}
