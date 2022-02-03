#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bdxmetroidentity
#' @importFrom shinyjs useShinyjs
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinyYM waiter_logo add_notie_deps
#' @noRd
#'

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    add_notie_deps(),
    shinyjs::useShinyjs(),
    
    waiter_logo(isinProd = golem::app_prod(), img_path = "www/LogoDataLab.png"),
    add_busy_spinner(spin = "fading-circle", color = "#ff4d3e", height = "150px", width = "150px"),
    
    fluidPage(
      navbarpage_bdx(
        title = "Elections",
        collapsible = TRUE,
        tabPanel(
          "Accueil",
          uiOutput(outputId = "my_logo"),
          
          selectizeInput(
            inputId = "type_elections",
            label = "Type d'election",
            choices = NULL,
            multiple = FALSE,
            options = list(deselectBehavior = "top")
          ),
          
          selectizeInput(
            inputId = "annee_elections",
            label = "Annee de l'election",
            choices = NULL,
            multiple = FALSE,
            options = list(deselectBehavior = "top")
          )
          
          
          # mod_accueil_ui("accueil_ui_1")
        ),
        tabPanel(
          "Occupation - observer 1 p\u00e9riode",
          # mod_occupation_1_periode_ui("occupation_ui_1")
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' 
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "elections"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
