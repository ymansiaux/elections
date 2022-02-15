#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @import sf
#' @importFrom shinyYM closeWaiter add_notie_alert
#' @importFrom xtradata xtradata_requete_features
#' @importFrom stringr str_extract str_sub
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate as_date
#' @importFrom janitor clean_names
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  options(datatable.print.class = TRUE)
  options(bitmapType = "cairo")
  
  observe(closeWaiter(golem::app_prod(), 3))
  
  mod_accueil_server("accueil_ui_1")
  mod_observer_1_election_server("observer_1_election_ui_1", data_elections = data_elections)
  mod_observer_plusieurs_elections_server("observer_plusieurs_elections_ui_1", data_elections = data_elections)
  
  #
  font_add_google(name = "Playfair Display", family = "Playfair Display")
  showtext_auto()
  
  
  #######################################
  # Chargement des données au démarrage #
  #######################################
  data_elections <- reactiveValues(data = NULL)
  
  observeEvent(session, {
    
    dat <- try(xtradata_requete_features(key = Sys.getenv("XTRADATA_KEY"), typename = "ST_PARK_P", showURL = TRUE))
    
    if(inherits(dat, "try-error")) {
      add_notie_alert(type = "error", text = "Echec de récupération des données",
                      stay = TRUE, time = 5, position = "bottom", session)
      
    } else {
      add_notie_alert(type = "success", text = "Connexion à la base OK",
                      stay = FALSE, time = 5, position = "bottom", session)
      
      #### A MODIFIER QUAND LES DONNEES SERONT SUR XTRADATA ####
      #data_elections$data <- dat
      data_elections$data <- copy(elections::sample_DACI_bdx) %>% 
        .[, TYPE_ELECTION := str_extract(string = NOM_ELECTION, pattern = "^[:alpha:]{1,}")] %>% 
        .[, DATE_ELECTION := as_date(DATE_ELECTION, format = "%d/%m/%Y")] %>% 
        .[, ANNEE_ELECTION := year(DATE_ELECTION)] %>% 
        .[, TYPE_ELECTION := stri_trans_general(str = TYPE_ELECTION, id = "Latin-ASCII")] %>%  #On vire les accents
        .[str_sub(TYPE_ELECTION, start=-1) == "s", TYPE_ELECTION := str_sub(TYPE_ELECTION, end=nchar(TYPE_ELECTION)-1)] %>% 
        .[, PRENOM := get_first_name(NOM_CANDIDAT)] %>% 
        .[, NOM := get_last_name(NOM_CANDIDAT, PRENOM)] %>% 
        .[NOM == "", NOM := PRENOM] %>% 
        .[, NOM_CANDIDAT_SHORT := str_sub(NOM_CANDIDAT,1,10)] %>% 
        clean_names()
        
    }
  }, ignoreNULL = FALSE, once = TRUE)
  
  # output$print_data <- renderPrint(
  #   head(data_elections$data)
  # )
  
  
  ### PARTIE BDXMETROIDENTITY ###
  rv <- reactiveValues()
  rv$theme <- "light"
  
  output$my_logo <- renderUI({
    if (rv$theme == "light") {
      tags$img(src = "www/datalab-logo-lightmode.png", width = "150px")
    } else if (rv$theme == "dark") {
      tags$img(src = "www/datalab-logo-darkmode.png", width = "150px")
    }
  })
}
