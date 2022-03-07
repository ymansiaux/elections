#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr mutate select filter group_by arrange distinct as_tibble summarise mutate_at vars ungroup inner_join rename
#' @import sf
#' @importFrom shinyYM closeWaiter add_notie_alert
#' @importFrom xtradata xtradata_requete_features
#' @importFrom stringr str_extract str_sub
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate as_date year
#' @importFrom janitor clean_names
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom shinyjs runjs
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  options(bitmapType = "cairo")
  
  observe(closeWaiter(golem::app_prod(), 3))
  
  debug_whereami <- TRUE
  
  mod_accueil_server("accueil_ui_1")
  
  # observeEvent(!is.null(data_elections$data), {
    
    mod_observer_1_election_resultats_globaux_server("observer_1_election_ui_1", data_elections = data_elections, debug_whereami = debug_whereami)
    
    mod_observer_1_election_resultats_selectionLVBV_server("observer_1_election_selection_LV_sur_carte_ui_1", data_elections = data_elections)
    
    mod_observer_1_election_resultats_1candidat_server("observer_1_candidat_ui_1", data_elections = data_elections)
    
    # mod_observer_plusieurs_elections_server("observer_plusieurs_elections_ui_1", data_elections = data_elections)
  # })
  
  #
  # font_add_google(name = "Playfair Display", family = "Playfair Display")
  font_add_google(name = "Nunito", family = "Nunito")
  showtext_auto()
  
  
  #######################################
  # Chargement des données au démarrage #
  #######################################
  data_elections <- reactiveValues(data = NULL)
  
  # observeEvent(session, {
  # browser()
  # 
  observeEvent(NULL, ignoreNULL = FALSE, ignoreInit = FALSE, once = TRUE, {
    runjs('$(".nav-link").addClass("disabled");');
    print("alerte1")
    add_notie_alert(type = "info", text = "Récupération des données ... Patience ...",
                    stay = FALSE, time = 5, position = "top", session)
    tictoc::tic()
    dat <- try(xtradata_requete_features(key = Sys.getenv("XTRADATA_KEY"), typename = "ST_PARK_P", showURL = TRUE))
    print(dat)
    tictoc::toc()
    
    if(inherits(dat, "try-error")) {
      print("alerte2")
      
      add_notie_alert(type = "error", text = "Echec de récupération des données",
                      stay = FALSE, time = 5, position = "bottom", session)
      
      
    } else {
      print("alerte3")
      
      add_notie_alert(type = "success", text = "Connexion à la base OK",
                      stay = FALSE, time = 5, position = "bottom", session)
      
      #### A MODIFIER QUAND LES DONNEES SERONT SUR XTRADATA ####
      #data_elections$data <- dat
      data_elections$data <-  as_tibble(elections::sample_DACI_bdx) %>% 
        mutate(
          DATE_ELECTION = as_date(DATE_EVENEMENT, format = "%d/%m/%Y"),
          ANNEE_ELECTION = year(DATE_ELECTION)) %>% 
        mutate(PRENOM = get_first_name(NOM_CANDIDAT)) %>% 
        mutate(NOM = get_last_name(NOM_CANDIDAT, PRENOM)) %>% 
        mutate(NOM = ifelse(NOM == "", PRENOM, NOM)) %>% 
        mutate(NOM_CANDIDAT_SHORT = str_sub(NOM_CANDIDAT,1,10)) %>% 
        select(-DATE_EVENEMENT) %>% 
        rename(NB_VOIX = VALEUR) %>% 
        clean_names()
      
      runjs('$(".nav-link").removeClass("disabled");');
      
    }
  })#, ignoreNULL = FALSE, once = TRUE)
  
  # output$print_data <- renderPrint(
  #   head(data_elections$data)
  # )
  
  
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
