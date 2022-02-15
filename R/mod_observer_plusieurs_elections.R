#' observer_plusieurs_elections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_observer_plusieurs_elections_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             actionButton(ns("pause"), "pause"),
             
             h1("titi")
             
             
      )
    )
    
  )
}

#' observer_plusieurs_elections Server Functions
#'
#' @noRd 
mod_observer_plusieurs_elections_server <- function(id, data_elections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$pause, {
      browser()
      
      t <- copy(data_elections$data)
      
      t <- copy(data_elections$data) %>% 
        .[, .(
          nb_voix = sum(nb_voix, na.rm = TRUE),
          nb_expr = sum(nb_expr, na.rm = TRUE)
        ),
        by = list(id_bureau = as.factor(id_bureau), date_election = as.integer(date_election))
        ] %>% 
        .[, pct := nb_voix / nb_expr] %>% 
        # .[, titi := as.factor("titi")] %>% 
        .[order(date_election)]
      
      
      library(ggstream)
      ggplot(t, aes(date_election, pct, fill = id_bureau)) +
        geom_stream()
      
      
      })
    
  })
}

## To be copied in the UI
# mod_observer_plusieurs_elections_ui("observer_plusieurs_elections_ui_1")

## To be copied in the server
# mod_observer_plusieurs_elections_server("observer_plusieurs_elections_ui_1")
