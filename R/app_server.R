#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @import sf
#' @importFrom shinyYM closeWaiter add_notie_alert
#' @importFrom xtradata xtradata_requete_features
#' @importFrom stringr str_extract
#' @importFrom lubridate as_date
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  options(datatable.print.class = TRUE)
  options(bitmapType = "cairo")
  
  observe(closeWaiter(golem::app_prod(), 3))
  
  
  ######
  type_elections <- reactive({
    req(data_elections$data)
    sort(
      unique(
        data_elections$data$TYPE_ELECTION
      )
    )
  }) #%>% debounce(100)
  
  annee_elections <- reactive({
    req(data_elections$data)
    req(input$type_elections)
    
    sort(
      unique(
        unlist(
          data_elections$data[TYPE_ELECTION %in% input$type_elections, "ANNEE_ELECTION"])
      )
    )
  })# %>% debounce(1000)
  
  
  observe({
    updateSelectizeInput(inputId = "type_elections",
                         choices = type_elections()
    )
  })
  
  observeEvent(input$type_elections, {
    updateSelectizeInput(inputId = "annee_elections",
                         choices = annee_elections()
    )
  })
  
  
  
  
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
        .[, ANNEE_ELECTION := year(DATE_ELECTION)]
      # corriger les libelles
      # virer les accents
      # si la dernière lettre est un s on la vire
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
