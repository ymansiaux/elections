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
                           tags$img(
                             src = "https://mdbcdn.b-cdn.net/img/new/standard/nature/181.webp",
                             class = "card-img-top",
                             alt = "Waterfall"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               "Card title 1"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
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
                           tags$img(
                             src = "https://mdbcdn.b-cdn.net/img/new/standard/nature/182.webp",
                             class = "card-img-top",
                             alt = "Sunset Over the Sea"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               "Card title 2"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
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
                           tags$img(
                             src = "https://mdbcdn.b-cdn.net/img/new/standard/nature/183.webp",
                             class = "card-img-top",
                             alt = "Sunset over the Sea"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               "Card title 3"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
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
