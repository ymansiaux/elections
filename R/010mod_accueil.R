#' accueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_accueil_ui <- function(id){
  ns <- NS(id)
  # tagList(
  #   fluidRow(
  #     column(width = 12,
  #            h1("titi"))
  #   )
  # )
  tagList(
    fluidRow(
      column(width = 12,
             tags$div(class = "container",
                      style = "text-align:center;",
                      
                      # https://mdbootstrap.com/docs/standard/extended/gallery/
                      tags$div(class = "row",
                               tags$div(class = "col-xs-12 col-4",
                                        tags$div(
                                          class = "card",
                                          # style = "height: 500px",
                                          style = "height:100%;",
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
                               
                               tags$div(class = "col-xs-12 col-4",
                                        tags$div(
                                          class = "card",
                                          # style = "height: 500px",
                                          style = "height:100%;",
                                          
                                          tags$img(
                                            src = "www/img/vote2.jpg",
                                            class = "card-img-top",
                                            style = "height:50%;",
                                            alt = "Waterfall"
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
                                              "Button 1",
                                              onclick="$('li:eq(1) a').tab('show');"
                                            )
                                          )
                                        )
                               ),
                               
                               tags$div(class = "col-xs-12 col-4",
                                        tags$div(
                                          class = "card",
                                          # style = "height: 500px",
                                          style = "height:100%;",
                                          
                                          tags$img(
                                            src = "www/img/vote3.jpg",
                                            class = "card-img-top",
                                            style = "height:50%;",
                                            alt = "Waterfall"
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
                                              "Button 1",
                                              onclick="$('li:eq(1) a').tab('show');"
                                            )
                                          )
                                        )
                               )
                      )
                      
             )
             
      )
    )
  )
  
  #            tags$div(
  #              id = "carouselMultiItemExample",
  #              class = "carousel slide carousel-dark text-center",
  #              `data-mdb-ride` = "carousel",
  
  #              tags$div(
  #                class = "carousel-inner py-4",
  #                tags$div(
  #                  class = "carousel-item active",
  #                  tags$div(
  #                    class = "container",
  #                    tags$div(
  #                      class = "row",
  #                      tags$div(
  #                        class = "col-4",
  #                        tags$div(
  #                          class = "card",
  #                          style = "height: 500px",
  #                          tags$img(
  #                            src = "www/img/vote1.jpg",
  #                            class = "card-img-top",
  #                            style = "height:50%;",
  #                            alt = "Waterfall"
  #                          ),
  #                          tags$div(
  #                            class = "card-body",
  #                            tags$h5(
  #                              class = "card-title",
  
  #                              "Résultats globaux"
  #                            ),
  #                            tags$p(
  #                              class = "card-text",
  #                              style = "height: 40%;",
  
  #                              "Observer les résultats complets d'une élection : voix obtenues pour chaque liste ou candidat, abstention"
  #                            ),
  #                            tags$a(
  #                              href = "#!",
  #                              class = "btn btn-primary",
  #                              "Button 1",
  #                              onclick="$('li:eq(1) a').tab('show');"
  #                            )
  #                          )
  #                        )
  #                      ),
  #                      tags$div(
  #                        class = "col-4",
  #                        tags$div(
  #                          class = "card",
  #                          style = "height: 500px",
  
  #                          tags$img(
  #                            src = "www/img/vote2.jpg",
  #                            class = "card-img-top",
  #                            style = "height:50%;",
  #                            alt = "Sunset Over the Sea"
  #                          ),
  #                          tags$div(
  #                            class = "card-body",
  #                            tags$h5(
  #                              class = "card-title",
  #                              "Résultats pour un lieu de vote"
  #                            ),
  #                            tags$p(
  #                              class = "card-text",
  #                              style = "height: 40%;",
  
  #                              "Résultats détaillés pour un lieu de vote et les bureaux qui y sont rattachés"
  #                            ),
  #                            tags$a(
  #                              href = "#!",
  #                              class = "btn btn-primary",
  #                              "Button 2",
  #                              onclick="$('li:eq(2) a').tab('show');"
  #                            )
  #                          )
  #                        )
  #                      ),
  #                      tags$div(
  #                        class = "col-4",
  #                        tags$div(
  #                          class = "card",
  #                          style = "height: 500px",
  
  #                          tags$img(
  #                            src = "www/img/vote3.jpg",
  #                            class = "card-img-top",
  #                            style = "height:50%;",
  #                            alt = "Sunset over the Sea"
  #                          ),
  #                          tags$div(
  #                            class = "card-body",
  #                            tags$h5(
  #                              class = "card-title",
  
  #                              "Résultats d'un candidat"
  #                            ),
  #                            tags$p(
  #                              class = "card-text",
  #                              style = "height: 40%;",
  
  #                              "Résultats détaillés d'un candidat dans les différents lieux et bureaux de vote"
  #                            ),
  #                            tags$a(
  #                              href = "#!",
  #                              class = "btn btn-primary",
  #                              "Button 3",
  #                              onclick="$('li:eq(3) a').tab('show');"
  #                            )
  #                          )
  #                        )
  #                      )
  #                    )
  #                  )
  #                )
  
  
  #              )
  #            )
  #     )
  #   )      
  # )
  
}


#' accueil Server Functions
#'
#' @noRd 
mod_accueil_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_accueil_ui("accueil_ui_1")

## To be copied in the server
# mod_accueil_server("accueil_ui_1")
