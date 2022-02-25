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
                 class = "d-flex justify-content-center mb-4",
                 tags$button(
                   class = "carousel-control-prev position-relative",
                   type = "button",
                   `data-mdb-target` = "#carouselMultiItemExample",
                   `data-mdb-slide` = "prev",
                   tags$span(
                     class = "carousel-control-prev-icon",
                     `aria-hidden` = "true"
                   ),
                   tags$span(
                     class = "visually-hidden",
                     "Previous"
                   )
                 ),
                 tags$button(
                   class = "carousel-control-next position-relative",
                   type = "button",
                   `data-mdb-target` = "#carouselMultiItemExample",
                   `data-mdb-slide` = "next",
                   tags$span(
                     class = "carousel-control-next-icon",
                     `aria-hidden` = "true"
                   ),
                   tags$span(
                     class = "visually-hidden",
                     "Next"
                   )
                 )
               ),
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
                               "Card title"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button"
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
                               "Card title"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button"
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
                               "Card title"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button"
                             )
                           )
                         )
                       )
                     )
                   )
                 ),
                 tags$div(
                   class = "carousel-item",
                   tags$div(
                     class = "container",
                     tags$div(
                       class = "row",
                       tags$div(
                         class = "col-lg-4 col-md-12",
                         tags$div(
                           class = "card",
                           tags$img(
                             src = "https://mdbcdn.b-cdn.net/img/new/standard/nature/184.webp",
                             class = "card-img-top",
                             alt = "Fissure in Sandstone"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               "Card title"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button"
                             )
                           )
                         )
                       ),
                       tags$div(
                         class = "col-lg-4 d-none d-lg-block",
                         tags$div(
                           class = "card",
                           tags$img(
                             src = "https://mdbcdn.b-cdn.net/img/new/standard/nature/185.webp",
                             class = "card-img-top",
                             alt = "Storm Clouds"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               "Card title"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button"
                             )
                           )
                         )
                       ),
                       tags$div(
                         class = "col-lg-4 d-none d-lg-block",
                         tags$div(
                           class = "card",
                           tags$img(
                             src = "https://mdbcdn.b-cdn.net/img/new/standard/nature/186.webp",
                             class = "card-img-top",
                             alt = "Hot Air Balloons"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               "Card title"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button"
                             )
                           )
                         )
                       )
                     )
                   )
                 ),
                 tags$div(
                   class = "carousel-item",
                   tags$div(
                     class = "container",
                     tags$div(
                       class = "row",
                       tags$div(
                         class = "col-lg-4 col-md-12 mb-4 mb-lg-0",
                         tags$div(
                           class = "card",
                           tags$img(
                             src = "https://mdbcdn.b-cdn.net/img/new/standard/nature/187.webp",
                             class = "card-img-top",
                             alt = "Peaks Against the Starry Sky"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               "Card title"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button"
                             )
                           )
                         )
                       ),
                       tags$div(
                         class = "col-lg-4 mb-4 mb-lg-0 d-none d-lg-block",
                         tags$div(
                           class = "card",
                           tags$img(
                             src = "https://mdbcdn.b-cdn.net/img/new/standard/nature/188.webp",
                             class = "card-img-top",
                             alt = "Bridge Over Water"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               "Card title"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button"
                             )
                           )
                         )
                       ),
                       tags$div(
                         class = "col-lg-4 mb-4 mb-lg-0 d-none d-lg-block",
                         tags$div(
                           class = "card",
                           tags$img(
                             src = "https://mdbcdn.b-cdn.net/img/new/standard/nature/189.webp",
                             class = "card-img-top",
                             alt = "Purbeck Heritage Coast"
                           ),
                           tags$div(
                             class = "card-body",
                             tags$h5(
                               class = "card-title",
                               "Card title"
                             ),
                             tags$p(
                               class = "card-text",
                               "Some quick example text to build on the card title and make up
                                    the bulk of the card's content."
                             ),
                             tags$a(
                               href = "#!",
                               class = "btn btn-primary",
                               "Button"
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
