#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @import sf
#' @importFrom shinyYM closeWaiter add_notie_alert
#' @importFrom xtradata xtradata_requete_features
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  options(datatable.print.class = TRUE)
  options(bitmapType = "cairo")
  
  observe(closeWaiter(golem::app_prod(), 3))
  
  
  # Chargement des données au démarrage
  data_elections <- reactiveValues(data = NULL)
  
  observeEvent(session, {
    
    dat <- try(xtradata_requete_features(key = Sys.getenv("XTRADATA_KEY"), typename = "ST_PARK_P", showURL = TRUE))

    if(inherits(dat, "try-error")) {
      add_notie_alert(type = "error", text = "Echec de récupération des données",
                      stay = TRUE, time = 5, position = "bottom", session)

    } else {
      add_notie_alert(type = "success", text = "Connexion à la base OK",
                      stay = FALSE, time = 5, position = "bottom", session)
      
      data_elections$data <- dat
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
