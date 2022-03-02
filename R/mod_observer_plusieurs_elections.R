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
    # https://mdbootstrap.com/docs/standard/extended/gallery/
    fluidRow(
      column(width = 12,
             tags$div(
               id = "carouselMultiItemExample",
               class = "carousel slide carousel-dark text-center",
               `data-mdb-ride` = "carousel",
               
               tags$div(
                 class = "carousel-inner py-4",
                 tags$div(
                   class = "carousel-item active",
                   tags$div(
                     class = "container",
                     tags$div(
                       class = "row",
                       tags$div(
                         class = "col-lg-4",
                         tags$div(
                           class = "card",
                           style = "height: 500px",
                           tags$img(
                             src = "www/img/vote1.jpg",
                             class = "card-img-top",
                             style = "height:50%;",
                             alt = "Waterfall"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               
                               "Résultats globaux"
                             ),
                             tags$p(
                               class = "card-text",
                               style = "height: 40%;",
                               
                               "Observer les résultats complets d'une élection : voix obtenues pour chaque liste ou candidat, abstention"
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button 1",
                               onclick="$('li:eq(1) a').tab('show');"
                             )
                           )
                         )
                       ),
                       tags$div(
                         class = "col-lg-4 d-none d-lg-block",
                         tags$div(
                           class = "card",
                           style = "height: 500px",
                           
                           tags$img(
                             src = "www/img/vote2.jpg",
                             class = "card-img-top",
                             style = "height:50%;",
                             alt = "Sunset Over the Sea"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               "Résultats pour un lieu de vote"
                             ),
                             tags$p(
                               class = "card-text",
                               style = "height: 40%;",
                               
                               "Résultats détaillés pour un lieu de vote et les bureaux qui y sont rattachés"
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button 2",
                               onclick="$('li:eq(2) a').tab('show');"
                             )
                           )
                         )
                       ),
                       tags$div(
                         class = "col-lg-4 d-none d-lg-block",
                         tags$div(
                           class = "card",
                           style = "height: 500px",
                           
                           tags$img(
                             src = "www/img/vote3.jpg",
                             class = "card-img-top",
                             style = "height:50%;",
                             alt = "Sunset over the Sea"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               
                               "Résultats d'un candidat"
                             ),
                             tags$p(
                               class = "card-text",
                               style = "height: 40%;",
                               
                               "Résultats détaillés d'un candidat dans les différents lieux et bureaux de vote"
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button 3",
                               onclick="$('li:eq(3) a').tab('show');"
                             )
                           )
                         )
                       )
                     )
                   )
                 )
                 
                 
               )
             )
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
    
    # observeEvent(input$pause, {
    #   browser()
    #   
    #   t <- copy(data_elections$data)
    #   
    #   t <- copy(data_elections$data) %>% 
    #     .[, .(
    #       nb_voix = sum(nb_voix, na.rm = TRUE),
    #       nb_expr = sum(nb_expr, na.rm = TRUE)
    #     ),
    #     by = list(id_bureau = as.factor(id_bureau), date_election = as.integer(date_election))
    #     ] %>% 
    #     .[, pct := nb_voix / nb_expr] %>% 
    #     # .[, titi := as.factor("titi")] %>% 
    #     .[order(date_election)]
    #   
    #   
    #   library(ggstream)
    #   ggplot(t, aes(date_election, pct, fill = id_bureau)) +
    #     geom_stream()
    #   
    #   
    #   })
    
  })
}

## To be copied in the UI
# mod_observer_plusieurs_elections_ui("observer_plusieurs_elections_ui_1")

## To be copied in the server
# mod_observer_plusieurs_elections_server("observer_plusieurs_elections_ui_1")
