#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bdxmetroidentity
#' @importFrom shinyjs useShinyjs
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinyYM waiter_logo add_notie_deps
#' @importFrom sass font_google
#' @noRd
#'

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    add_notie_deps(),
    useShinyjs(),
    
    waiter_logo(isinProd = golem::app_prod(), img_path = "www/LogoDataLab.png"),
    
    navbarPage(
      
      theme = theme_bdxmetro_shiny(
        bg = "#3FD2C7",
        fg = "white",
        base_font = font_google("Nunito"), 
        heading_font = font_google("Nunito")
      ),
      
      title = "Elections",
      collapsible = TRUE,
      
      footer = includeHTML(app_sys("app/www/footer.html")),
      
      tabPanel(
        "Accueil",
        div(class = "content",
            mod_accueil_ui("accueil_ui_1")
        )
      ),
      
      tabPanel(
        "Observer 1 élection",
        div(class = "content",
        mod_observer_1_election_resultats_globaux_ui("observer_1_election_ui_1")
        )
      ),
      
      tabPanel(
        "Observer 1 BV ou 1 LV d'1 élection",
        mod_observer_1_election_resultats_selectionLVBV_ui("observer_1_election_selection_LV_sur_carte_ui_1")
      ),
      
      tabPanel(
        "Observer 1 candidat",
        mod_observer_1_election_resultats_1candidat_ui("observer_1_candidat_ui_1")
      )#,
      
      # tabPanel(
      #   "Observer plusieurs élections",
      #   mod_observer_plusieurs_elections_ui("observer_plusieurs_elections_ui_1")
      # )
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
